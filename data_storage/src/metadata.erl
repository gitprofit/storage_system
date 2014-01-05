%% @author Michal
%% @doc Local files metadata storage (memory / disk)


-module(metadata).
-include("shared.hrl").

%%
%% @TODO change PK: file_id to { user_id, v_path }
%% @TODO remove file_id completely
%%

%% ====================================================================
%% API functions
%% ====================================================================
-export([init/1,
		 deinit/0,
		 dump/0,
		 add/1,
		 update/1,
		 get_by_id/1,
		 remove/1,
		 get_by_user/1,
		 to_list/0]).

init(FileMetaLocation) ->
	ets:new(memDb, [named_table, {keypos, #file.id}]),
	dets:open_file(perDb, [
						   	{file, FileMetaLocation },
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

remove(FileId) ->
	ets:delete(memDb, FileId),
	dets:delete(perDb, FileId).

get_by_user(UserId) ->
	ets:match_object(memDb, #file{owner_id=UserId, _='_'}).

to_list() ->
	ets:tab2list(memDb).

dump() ->
	ets:foldl(fun(Elem, _Acc) ->
					  Name = Elem#file.v_path,
					  Id = Elem#file.id,
					  io:format("file ~s as ~s~n", [Name, Id]),
					  _Acc
			  end, [], memDb).

%% ====================================================================
%% Internal functions
%% ====================================================================
