%% @author Michal
%% @doc Local files metadata storage (memory / disk)


-module(metadata).
-include("shared.hrl").
-define(STORAGE_DIR, "P:\\local_ds_meta\\").

%% ====================================================================
%% API functions
%% ====================================================================
-export([init/0,
		 deinit/0,
		 dump/0,
		 add/1,
		 update/1,
		 get_by_id/1,
		 get_by_user/1]).

init() ->
	ets:new(memDb, [named_table, {keypos, #file.id}]),
	dets:open_file(perDb, [
						   	{file, ?STORAGE_DIR ++ atom_to_list(node()) ++ ".metadata" },
						  	{keypos, #file.id}
					]),
	dets:to_ets(perDb, memDb),
	ok.

deinit() ->
	ets:delete(memDb),
	dets:close(perDb),
	ok.

add(#file{} = File) ->
	InsFile = File#file{id=uuid:generate()},
	%% @TODO init other fields
	update(InsFile).

update(#file{} = File) ->
	ets:insert(memDb, File),
	dets:insert(perDb, File),
	{ ok, File }.

get_by_id(FileId) ->
	case ets:lookup(memDb, FileId) of
		[File]	-> { ok, File };
		[]		-> { error, not_found }
	end.

get_by_user(_UserId) ->
	%% @TODO implement
	{ error, not_implemented }.

dump() ->
	ets:foldl(fun(Elem, _Acc) ->
					  Name = Elem#file.name,
					  Id = Elem#file.id,
					  io:format("file ~s as ~s~n", [Name, Id]),
					  _Acc
			  end, [], memDb).

%% ====================================================================
%% Internal functions
%% ====================================================================
