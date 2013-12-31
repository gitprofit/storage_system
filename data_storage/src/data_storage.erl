%% @author Michal
%% @doc Data Storage Node

-module(data_storage).

%% ====================================================================
%% API functions
%% ====================================================================
-export([start/0, stop/0]).

start() ->
	register(storage,
			 spawn_link(fun init/0)).

stop() ->
	storage ! { self(), stop }.

%% ====================================================================
%% Internal functions
%% ====================================================================

init() ->
	ets:new(localFiles, [named_table]),
	loop(0).

loop(Index) ->
	receive
		{ _Pid, stop } ->
			io:format("bye. luv ja.~n");
		{ Pid, list } ->
			io:format("listing here~n"),
			Pid ! { ok,
					ets:foldl(fun(Elem, Acc) -> [Elem | Acc] end, [], localFiles)
				  },
			loop(Index);
		{ Pid, add, File } ->
			io:format("new file iz ~w~n", [File]),
			ets:insert(localFiles, {Index, File}),
			Pid ! { ok, added },
			loop(Index+1);
		_Other ->
			io:format("server got: ~w~n", [_Other]),
			loop(Index)
	end.
