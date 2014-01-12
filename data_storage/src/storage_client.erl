%% @author Michal
%% @doc 


-module(storage_client).
-include("shared.hrl").
-define(HANDLERS, proc_handlers).

%% ====================================================================
%% API functions
%% ====================================================================
-export([
		 start/1,
		 stop/0,
		 create/1,
		 delete/1,
		 pull/1,
		 write/1,
		 rename/2,
		 scan/0,
		 create2/1,
		 delete2/1,
		 pull2/1,
		 write2/1,
		 rename2/2,
		 scan2/0
		]).

%%
%% @TODO handle operations concurrently !!!
%%

start([GatewayNode, UserId, MountPoint]) ->
	globals:init(),
	globals:set(gateway, GatewayNode),
	globals:set(userId, UserId),
	globals:set(root, MountPoint),
	ets:new(?HANDLERS, [named_table, public, {heir, whereis(init), nothing} ]).

stop() ->
	ets:delete(?HANDLERS),
	globals:deinit().


force_read(Path) ->
	case file:read_file(Path) of
		{ ok, RawData } ->
			{ ok, RawData };
		{ error, eacces } ->
			timer:sleep(500),
			force_read(Path);
		{ error, _ } = Msg -> Msg
	end.


create(VPath) ->
	case force_read(resolve(VPath)) of
		{ ok, RawData } ->
			io:format("got file ~s!, sening create ...~n", [VPath]),
			send_request(#request{	action	= create,
									user_id	= user(),
									v_path	= VPath,
									options	= #create_opts{	data = RawData }
								});
		{ error, Err } ->
			io:format("create error! ~w on: ~s (~s)~n", [Err, VPath, resolve(VPath)]),
			{ error, not_found }
	end.


delete(VPath) ->
	io:format("got file ~s!, sening delete ...~n", [VPath]),
	send_request(#request{	action	= delete,
							user_id	= user(),
							v_path	= VPath
						}).


write(VPath) ->
	case force_read(resolve(VPath)) of
		{ ok, RawData } ->
			io:format("got file ~s!, sening write ...~n", [VPath]),
			send_request(#request{	action	= write,
									user_id	= user(),
									v_path	= VPath,
									options	= #write_opts{ data = RawData }
								});
		{ error, Err } ->
			io:format("write error! ~w on: ~s (~s)~n", [Err, VPath, resolve(VPath)]),
			{ error, not_found }
	end.

rename(VPathFrom, VPathTo) ->
	io:format("got file ~s!, sening write (rename) ...~n", [VPathFrom]),
	send_request(#request{	action	= write,
							user_id	= user(),
							v_path	= VPathFrom,
							options	= #write_opts{ v_path = VPathTo }
						}).

pull(VPath) ->
	io:format("got pull request ~s!~n", [VPath]),
	{ ok, RawData } = send_request(#request{	action	= read,
												user_id	= user(),
												v_path	= VPath
										   }),
	io:format("pulled ~w bytes!~n", [byte_size(RawData)]),
	RealPath = resolve(VPath),
	io:format("path resolved to ~s~n", [RealPath]),
	filelib:ensure_dir(RealPath),
	io:format("dir ensured!~n"),
	file:write_file(RealPath, RawData),
	io:format("writing done!~n"),
	{ok, pulled }.
	

scan() ->
	io:format("scan requested!~n"),
	send_request(#request{ action	= list,
						   user_id	= user(),
						   broadcast = true
						 }).


send_request(#request{} = Req) ->
	{ ?STORAGE_PROC, gateway() } ! { self(), Req },
	receive
		{ ok, Result } -> { ok, Result };
		{ error, Err } -> { error, Err }
	after 50000 ->
			io:format("TIMED OUT ON: ~s~n", [Req#request.v_path]),
			{ error, timeout }
	end.






%%
%% NEW action impls, using async handlers
%% TODO remove old ones
%%




create2(VPath) ->
	case force_read(resolve(VPath)) of
		{ ok, RawData } ->
			io:format("got file ~s!, sening create ...~n", [VPath]),
			push_request(#request{	action	= create,
									user_id	= user(),
									v_path	= VPath,
									options	= #create_opts{	data = RawData }
								});
		{ error, Err } ->
			io:format("create error! ~w on: ~s (~s)~n", [Err, VPath, resolve(VPath)]),
			{ error, not_found }
	end.


delete2(VPath) ->
	io:format("got file ~s!, sening delete ...~n", [VPath]),
	push_request(#request{	action	= delete,
							user_id	= user(),
							v_path	= VPath
						}).


write2(VPath) ->
	case force_read(resolve(VPath)) of
		{ ok, RawData } ->
			io:format("got file ~s!, sening write ...~n", [VPath]),
			push_request(#request{	action	= write,
									user_id	= user(),
									v_path	= VPath,
									options	= #write_opts{ data = RawData }
								});
		{ error, Err } ->
			io:format("write error! ~w on: ~s (~s)~n", [Err, VPath, resolve(VPath)]),
			{ error, not_found }
	end.

rename2(VPathFrom, VPathTo) ->
	io:format("got file ~s!, sening write (rename) ...~n", [VPathFrom]),
	push_request(#request{	action	= write,
							user_id	= user(),
							v_path	= VPathFrom,
							options	= #write_opts{ v_path = VPathTo }
						}).

pull2(VPath) ->
	io:format("got pull request ~s!~n", [VPath]),
	{ ok, RawData } = await_request(#request{	action	= read,
												user_id	= user(),
												v_path	= VPath
										   }),
	io:format("pulled ~w bytes!~n", [byte_size(RawData)]),
	RealPath = resolve(VPath),
	io:format("path resolved to ~s~n", [RealPath]),
	filelib:ensure_dir(RealPath),
	io:format("dir ensured!~n"),
	file:write_file(RealPath, RawData),
	io:format("writing done!~n"),
	{ok, pulled }.
	

scan2() ->
	io:format("scan requested!~n"),
	await_request(#request{ action	= list,
						   user_id	= user(),
						   broadcast = true
						 }).










%% @def thread method (every file has one)
async_request_handler() ->
	receive
		{ _Pid, handle, #request{} = Req } ->
			send_request(Req);
		{ Pid, await, #request{} = Req } ->
			Pid ! send_request(Req)
	end,
	async_request_handler().

%% @def pushes request to associated async_request_handler and returns immediately
push_request(#request{v_path = VPath} = Req) ->
	get_handler(VPath) ! { self(), handle, Req },
	{ ok, request_pushed }.

%% @def pushes request and waits for response
await_request(#request{v_path = VPath} = Req) ->
	get_handler(VPath) ! { self(), await, Req },
	receive
		Response -> Response
	end.

%% @def retrieves (or creates) handler for given file
get_handler(VPath) ->
	case ets:lookup(?HANDLERS, VPath) of
		[{VPath, HandlerPid}] -> HandlerPid;
		[] -> ets:insert(?HANDLERS, { VPath, spawn(fun async_request_handler/0) }),
			  get_handler(VPath);
		_ -> { error, handler_not_found }
	end.

%% ====================================================================
%% Internal functions
%% ====================================================================

resolve(RelativePath) ->
	filename:join([globals:get(root), RelativePath]).

user() ->
	globals:get(userId).

gateway() ->
	list_to_atom(globals:get(gateway)).
