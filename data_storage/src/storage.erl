%% @author Michal
%% @doc Data Storage request processor module


-module(storage).
-include("shared.hrl").
-define(WORK_DIR, "P:\\local_ds_meta\\").
-define(NODE_DIR, ?WORK_DIR ++ atom_to_list(node()) ++ "\\").

%% ====================================================================
%% API functions
%% ====================================================================
-export([start/0, stop/0]).

start() ->
	register(?STORAGE_PROC,
			 spawn_link(fun init/0)).

stop() ->
	?STORAGE_PROC ! { self(), stop }.

%% ====================================================================
%% Internal functions
%% ====================================================================

init() ->
	NodeDir = ?NODE_DIR,
	filelib:ensure_dir(NodeDir),
	metadata:init(NodeDir++".metadata"),
	
	%% calculate fill
	Files = metadata:to_list(),
	Fill = lists:foldl(fun(File, Acc) ->
							   Acc + File#file.size
					   end, 0, Files),

	globals:set(fill, Fill),
	
	io:format("node ~s, fill ~w/~w with ~w files~n",
			  [node(), globals:get(fill), globals:get(capacity), length(Files)]),

	loop().

deinit() ->
	io:format("bye. luv ja.~n"),
	metadata:deinit().


%%
%% TODO consider merging create into write
%%

%%
%% TODO handle broadcasts in another thread
%% 


loop() ->
	receive
		{ _Pid, stop } -> ok;
		
		%% create handled separately
		{ Pid, #request{action	= create,
						options	= #create_opts{data			= Data,
											   force_loc	= ForceLoc}
					   } = Req } -> 
			
			ReqCap = byte_size(Data),
			
			io:format("got create request! needs ~w bytes~n", [ReqCap]),
			
			case { ForceLoc, ReqCap =< (globals:get(capacity)-globals:get(fill)) } of
				{ true, true } ->
					%% place here
					Pid ! process_request(Req);
				{ _, _ } ->
					%% find best location
					NewOpt = Req#request.options#create_opts{force_loc=true},
					NewReq = Req#request{options=NewOpt},
					case system:pref_storage(ReqCap) of
						{ undefined, _ } -> io:format("system full! request lost!~n");
						{ Node, _ } -> 
							io:format("dispatching to: ~s~n", [Node]),
							{ ?STORAGE_PROC, Node} ! { Pid, NewReq }
					end
			end,

			loop();
		
		%% list handled separately
		{ Pid, #request{action		= list,
						user_id		= UserId,
						broadcast	= Brdc
					   } = Req } ->
			MyEntries = lists:map(fun(#file{v_path=VPath, last_access=Time}) ->
										  { VPath, Time }
								  end,
								  metadata:get(UserId)),
			case Brdc of
				true -> Pid ! { ok, system:scan(Req#request{broadcast=false})++MyEntries};
				false -> Pid ! { ok, MyEntries }
			end,
			loop();
		
		%% request = read | write | delete | move
		{ Pid, #request{broadcast	= Brdc,
						user_id		= UserId,
						v_path		= VPath } = Req } ->
			case { metadata:get(UserId, VPath), Brdc } of
				{ { error, not_found }, true } ->
					io:format("REQUEST: broadcast & not found, ~s~n", [VPath]),
					system:broadcast(?STORAGE_PROC, { Pid, Req#request{broadcast=false} });
				{ { error, _Err }, _ } ->
					io:format("REQUEST: error, do not broadcast, ~s~n", [VPath]),
					ok;
				{ { ok, _ }, _ } -> 
					io:format("REQUEST: found, , ~s~n", [VPath]),
					Pid ! process_request(Req) 
			end,
			loop();
		
		_Other ->
			io:format("storage got: ~w~n", [_Other]),
			loop()
	end,
	deinit().


%%
%% TODO permission checking !!!!!
%%


process_request(#request{action		= create,
						 user_id	= UserId,
						 v_path		= VPath,
						 options	= #create_opts{ data = Data }
						}) ->
	io:format("processing create ...~n"),
	io:format("new file iz ~s~n", [VPath]),
	
	File = #file{owner_id		= UserId,
				 last_access	= util:timestamp(),
				 size			= byte_size(Data),
				 v_path			= VPath},
	
	{ ok, #file{local_id=NewId} } = metadata:create(File),
	globals:set(fill, globals:get(fill)+byte_size(Data)),
	
	file:write_file(?NODE_DIR++NewId, Data),
	
	{ ok, created };



process_request(#request{action		= delete,
						 user_id	= UserId,
						 v_path		= VPath
						}) ->
	io:format("processing delete ...~n"),
	io:format("id iz ~s~n", [VPath]),

	{ ok, File } = metadata:get(UserId, VPath),
	globals:set(fill, globals:get(fill)-File#file.size),
	
	metadata:delete(File),
	file:delete(?NODE_DIR++File#file.local_id),
	
	{ ok, deleted };



process_request(#request{action		= read,
						 user_id	= UserId,
						 v_path		= VPath
						}) ->
	io:format("processing read ...~n"),
	{ ok, File } = metadata:get(UserId, VPath),
	io:format("got file ~s~n", [File#file.v_path]),
	metadata:modify(File#file{last_access=util:timestamp()}),
	io:format("meta modified!~n"),
	{ ok, Data } = file:read_file(?NODE_DIR++File#file.local_id),
	io:format("got ~w bytes!~n", [byte_size(Data)]),
	{ok, Data };



process_request(#request{action		= write,
						 user_id	= UserId,
						 v_path		= VPath,
						 options	= #write_opts{data = Data,
												  v_path = VPath2 }
						}) ->
	io:format("processing write ~s ...~n", [VPath]),
	
	
	{ ok, File } = metadata:get(UserId, VPath),
	io:format("retrieved as: ~s~n", [File#file.local_id]),
	%%timer:sleep(1000),
	
	NewVPath = case VPath2 of
				   false -> File#file.v_path;
				   _ -> VPath2
			   end,
	io:format("path set to: ~s~n", [NewVPath]),
	
	NewSize = case Data of
				  false -> File#file.size;
				   _ -> globals:set(fill, globals:get(fill)-File#file.size),
						globals:set(fill, globals:get(fill)+byte_size(Data)), 
						byte_size(Data)
			   end,
	io:format("size set to: ~w~n", [NewSize]),
	
	NewFile = File#file{last_access		= util:timestamp(),
						size			= NewSize,
						v_path			= NewVPath},
	
	metadata:modify(NewFile),
	
	case Data of
		false -> io:format("data not changed~n"), ok;
		_ -> io:format("writing data~n"),
			 file:write_file(?NODE_DIR++NewFile#file.local_id, Data)
	end,
	io:format("writing done!!!!!!!!!!!!!!~n"),
	{ ok, changes_written };



process_request(#request{action		= move
						}) ->
	io:format("processing move ...~n"),
	{error, not_supported }.


