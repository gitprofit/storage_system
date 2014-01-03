%% @author Michal
%% @doc Communication and synchronization module


-module(system).
-include("shared.hrl").
-define(NODE_TAB, remoteNodes).
%% ====================================================================
%% API functions
%% ====================================================================
-export([start/0, start/1, stop/0, nodes/0, broadcast/2]).

start() ->
	register(?SYSTEM_PROC,
			 spawn_link(fun init/0)).

start(InitialNode) ->
	register(?SYSTEM_PROC,
			 spawn_link(fun()->init(InitialNode) end)).

stop() ->
	?SYSTEM_PROC ! { self(), stop }.

nodes() ->
	ets:foldl(fun({Node}, _Acc) ->
					  io:format("~s~n", [Node]),
					  _Acc
			  end, [], ?NODE_TAB).

broadcast(Proc, Msg) ->
	ets:foldl(fun({Node}, _Acc) ->
					  { Proc, Node } ! Msg,
					  _Acc
			  end, [], ?NODE_TAB).

%% ====================================================================
%% Internal functions
%% ====================================================================

init() ->
	ets:new(?NODE_TAB, [named_table]),
	loop().

init(InitialNode) ->
	ets:new(?NODE_TAB, [named_table]),
	ets:insert(?NODE_TAB, {InitialNode}),
	update_nodes(),
	say_hello(),
	loop().

deinit() ->
	ets:delete(remoteNodes),
	ok.

%% @doc Scans all known remote nodes to find about new ones
update_nodes() ->
	Remotes =
	ets:foldl(fun({Node}, Acc) ->
					  { ?SYSTEM_PROC, Node } ! { self(), request_nodes },
					  receive
						  { ok, Nodes } -> Acc ++ Nodes;
						  _ -> Acc
					  after ?TIMEOUT -> Acc
					  end
			  end,
			  [],
			  ?NODE_TAB),
	ets:insert(?NODE_TAB, Remotes),
	ets:delete(?NODE_TAB, node()).

%% @doc Broadcasts own pid to all nodes in the system
say_hello() ->
	ets:foldl(fun({Node}, _Acc) ->
					  { ?SYSTEM_PROC, Node } ! { self(), hello }
			  end,
			  [],
			  ?NODE_TAB).

loop() ->
	receive
		{ _Pid, stop } ->
			io:format("all systems offlie.~n"),
			deinit();
		
		{ Pid, hello } ->
			ets:insert(?NODE_TAB, {node(Pid)}),
			loop();
		
		{ Pid, request_nodes } ->
			ets:insert(?NODE_TAB, {node(Pid)}),
			io:format("nodes list served.~n"),
			Pid ! { ok, ets:tab2list(?NODE_TAB) },
			loop();
		
		_Other ->
			io:format("system got: ~w~n", [_Other]),
			loop()
	end.
