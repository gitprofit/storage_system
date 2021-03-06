%% @author Michal
%% @doc Data Storage request processor module


-module(storage).
-include("shared.hrl").
-define(WORK_DIR, "P:\\local_ds_meta\\").
-define(NODE_DIR, ?WORK_DIR ++ atom_to_list(node()) ++ "\\").
-define(EXEC_PROC, executor_proc).
-define(EXECUTORS, proc_executors).
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
	
	register(?EXEC_PROC, spawn_link(fun executor/0)),
	ets:new(?EXECUTORS, [named_table, public, {heir, whereis(init), nothing} ]),
	
	io:format("~w: node ~s, fill ~w/~w with ~w files~n",
			  [erlang:localtime(), node(), globals:get(fill), globals:get(capacity), length(Files)]),

	loop().

deinit() ->
	io:format("~w: bye. luv ja.~n", [erlang:localtime()]),
	ets:delete(?EXECUTORS),
	metadata:deinit().


%%
%% TODO consider merging create into write
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
			
			io:format("~w: got create request! needs ~w bytes~n", [erlang:localtime(), ReqCap]),
			
			case { ForceLoc, ReqCap =< (globals:get(capacity)-globals:get(fill)) } of
				{ true, _ } -> %% TODO fix create chain: if it's forced, space has been reserved
					%% place here
					%Pid ! process_request(Req);
					%?EXEC_PROC ! { Pid, Req };
					push_request(Pid, Req);
				{ _, _ } ->
					%% find best location
					NewOpt = Req#request.options#create_opts{force_loc=true},
					NewReq = Req#request{options=NewOpt},
					case system:pref_storage(ReqCap) of
						{ undefined, _ } -> io:format("~w: system full! request lost!~n", [erlang:localtime()]);
						{ Node, _ } -> 
							io:format("~w: dispatching to: ~s~n", [erlang:localtime(), Node]),
							{ ?SYSTEM_PROC, Node} ! { self(), reserve_storage, ReqCap },
							receive
								{ ok, reserved } -> ok
							end,
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
					io:format("~w: REQUEST: broadcast & not found, ~s~n", [erlang:localtime(), VPath]),
					system:broadcast(?STORAGE_PROC, { Pid, Req#request{broadcast=false} });
				{ { error, _Err }, _ } ->
					io:format("~w: REQUEST: no broadcast & not found, ~s~n", [erlang:localtime(), VPath]),
					ok;
				{ { ok, _ }, _ } -> 
					io:format("~w: REQUEST: found, , ~s~n", [erlang:localtime(), VPath]),
					%Pid ! process_request(Req) 
					%?EXEC_PROC ! { Pid, Req }
					push_request(Pid, Req)
			end,
			loop();
		
		_Other ->
			io:format("~w: storage got: ~w~n", [erlang:localtime(), _Other]),
			loop()
	end,
	deinit().









%% @def Executor that handles and processes local requests, thread method (every file has one)
executor() ->
	receive
		{ Pid, #request{} = Req } ->
			Pid ! process_request(Req)
	end,
	executor().

%% @def pushes request to associated executor and returns immediately
push_request(Pid, #request{user_id = UserId, v_path = VPath} = Req) ->
	get_executor(UserId++VPath) ! { Pid, Req },
	{ ok, request_pushed }.

%% @def retrieves (or creates) handler for given file
get_executor(Name) ->
	case ets:lookup(?EXECUTORS, Name) of
		[{Name, ExecutorPid}] -> ExecutorPid;
		[] -> ets:insert(?EXECUTORS, { Name, spawn(fun executor/0) }),
			  get_executor(Name);
		_ -> { error, handler_not_found }
	end.





%%
%% TODO permission checking !!!!!
%%


process_request(#request{action		= create,
						 user_id	= UserId,
						 v_path		= VPath,
						 options	= #create_opts{ data = Data }
						}) ->
	io:format("~w: processing create ...~n", [erlang:localtime()]),
	io:format("~w: new file iz ~s~n", [erlang:localtime(), VPath]),
	
	File = #file{owner_id		= UserId,
				 last_access	= util:timestamp(),
				 size			= byte_size(Data),
				 v_path			= VPath},
	
	{ ok, #file{local_id=NewId} } = metadata:create(File),
	%% storage reserved on dispatching
	%% globals:set(fill, globals:get(fill)+byte_size(Data)),
	
	file:write_file(?NODE_DIR++NewId, Data),
	
	io:format("~w: creating done ...~n", [erlang:localtime()]),
	
	{ ok, created };



process_request(#request{action		= delete,
						 user_id	= UserId,
						 v_path		= VPath
						}) ->
	io:format("~w: processing delete ...~n",[erlang:localtime()]),
	io:format("~w: id iz ~s~n", [erlang:localtime(), VPath]),

	{ ok, File } = metadata:get(UserId, VPath),
	globals:set(fill, globals:get(fill)-File#file.size),
	
	metadata:delete(File),
	file:delete(?NODE_DIR++File#file.local_id),
	
	{ ok, deleted };



process_request(#request{action		= read,
						 user_id	= UserId,
						 v_path		= VPath
						}) ->
	io:format("~w: processing read ...~n", [erlang:localtime()]),
	{ ok, File } = metadata:get(UserId, VPath),
	io:format("~w: got file ~s~n", [erlang:localtime(), File#file.v_path]),
	metadata:modify(File#file{last_access=util:timestamp()}),
	io:format("~w: meta modified!~n", [erlang:localtime()]),
	{ ok, Data } = file:read_file(?NODE_DIR++File#file.local_id),
	io:format("~w: got ~w bytes!~n", [erlang:localtime(), byte_size(Data)]),
	{ok, Data };



process_request(#request{action		= write,
						 user_id	= UserId,
						 v_path		= VPath,
						 options	= #write_opts{data = Data,
												  v_path = VPath2 }
						}) ->
	io:format("~w: processing write ~s ...~n", [erlang:localtime(), VPath]),
	
	
	{ ok, File } = metadata:get(UserId, VPath),
	io:format("~w: retrieved as: ~s~n", [erlang:localtime(), File#file.local_id]),
	%%timer:sleep(1000),
	
	NewVPath = case VPath2 of
				   false -> File#file.v_path;
				   _ -> VPath2
			   end,
	io:format("~w: path set to: ~s~n", [erlang:localtime(), NewVPath]),
	
	NewSize = case Data of
				  false -> File#file.size;
				   _ -> globals:set(fill, globals:get(fill)-File#file.size),
						globals:set(fill, globals:get(fill)+byte_size(Data)), 
						byte_size(Data)
			   end,
	io:format("~w: size set to: ~w~n", [erlang:localtime(), NewSize]),
	
	NewFile = File#file{last_access		= util:timestamp(),
						size			= NewSize,
						v_path			= NewVPath},
	
	metadata:modify(NewFile),
	
	case Data of
		false -> ok;%%io:format("~w: data not changed~n", [erlang:localtime()]), ok;
		_ -> io:format("~w: writing data~n", [erlang:localtime()]),
			 file:write_file(?NODE_DIR++NewFile#file.local_id, Data)
	end,
	io:format("~w: writing done!!!!!!!!!!!!!!~n", [erlang:localtime()]),
	{ ok, changes_written };



process_request(#request{action		= move
						}) ->
	io:format("~w: processing move ...~n", [erlang:localtime()]),
	{error, not_supported }.


