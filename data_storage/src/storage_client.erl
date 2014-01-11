%% @author Michal
%% @doc 


-module(storage_client).
-include("shared.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
-export([
		 start/1,
		 stop/0,
		 create/1,
		 delete/1,
		 %%read/1,
		 write/1,
		 rename/2
		 %%grab_ids/2
		]).

%%
%% @TODO handle operations concurrently !!!
%%

start([GatewayNode, UserId, MountPoint]) ->
	globals:init(),
	globals:set(gateway, GatewayNode),
	globals:set(userId, UserId),
	globals:set(root, MountPoint).

stop() ->
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
	


send_request(#request{} = Req) ->
	{ ?STORAGE_PROC, gateway() } ! { self(), Req },
	receive
		{ ok, Result } -> Result;
		{ error, Err } -> { error, Err }
	after 50000 ->
			io:format("TIMED OUT ON: ~s~n", [Req#request.v_path]),
			{ error, timeout }
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
