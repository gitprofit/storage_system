%% @author Michal
%% @doc Data Storage request processor module


-module(storage).
-include("shared.hrl").
-define(WORK_DIR, "P:\\local_ds_meta\\").

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
	NodeDir = ?WORK_DIR ++ atom_to_list(node()) ++ "\\",
	filelib:ensure_dir(NodeDir),
	metadata:init(NodeDir++".metadata"),
	loop().

deinit() ->
	metadata:deinit().

loop() ->
	receive
		{ _Pid, stop } ->
			io:format("bye. luv ja.~n"),
			deinit();
		
		{ Pid, #request{action=create} = Req } -> % create handled separately
			Pid ! process_request(Req);
		
		{ Pid, #request{} = Req } ->
			case metadata:get_by_id(Req#request.file_id) of
				{ error, not_found } ->
					system:broadcast(?STORAGE_PROC, { Pid, Req });
				{ ok, _ } -> 
					Pid ! process_request(Req)
			end,
			loop();
		
		_Other ->
			io:format("storage got: ~w~n", [_Other]),
			loop()
	end.


process_request(#request{action=create,
						 file_id=FileId,
						 user_id=UserId}) ->
	io:format("processing create ...~n"),
	io:format("new file iz ~s~n", [FileId]),
	File = #file{name=FileId,
				 owner_id=UserId,
				 last_access=calendar:universal_time()},
	{ ok, #file{id=NewId} } = metadata:add(File),
	{ ok, NewId };

process_request(#request{action=delete,
						 file_id=FileId}) ->
	io:format("processing delete ...~n"),
	metadata:remove(FileId),
	{ ok, deleted };

process_request(#request{action=read,
						 file_id=FileId}) ->
	io:format("processing read ...~n"),
	{ ok, File } = metadata:get_by_id(FileId),
	{ ok, File#file.name };

process_request(#request{action=write,
						 file_id=_FileId}) ->
	io:format("processing write ...~n"),
	{ ok, changes_written }.
