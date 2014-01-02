%% @author Michal
%% @doc Communication and synchronization module


-module(system).
-include("shared.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
-export([start/0, stop/0]).

start() ->
	register(?SYSTEM_PROC,
			 spawn_link(fun init/0)).

stop() ->
	?SYSTEM_PROC ! { self(), stop }.

%% ====================================================================
%% Internal functions
%% ====================================================================

init() ->
	ets:new(remoteNodes, [named_table]),
	loop().

deinit() ->
	ets:delete(remoteNodes),
	ok.

loop() ->
	receive
		{ _Pid, stop } ->
			io:format("all systems offlie.~n"),
			deinit();
		_Other ->
			io:format("system got: ~w~n", [_Other]),
			loop()
	end.
