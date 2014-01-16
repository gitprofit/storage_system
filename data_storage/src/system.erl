%% @author Michal
%% @doc Communication and synchronization module


-module(system).
-include("shared.hrl").
-define(NODE_TAB, remoteNodes).
%% ====================================================================
%% API functions
%% ====================================================================
-export([start/1,
		 stop/0,
		 nodes/0,
		 broadcast/2,
		 pref_storage/1,
		 scan/1
		]).

start(InitialNodes) ->
	register(?SYSTEM_PROC,
			 spawn_link(fun()->init(InitialNodes) end)).

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

%% @doc Returns most preferred node capable of storing RequiredCap bytes
pref_storage(RequiredCap) ->
	%% send
	lists:foldl(fun({Node}, _Acc) ->
						{ ?SYSTEM_PROC, Node } ! { self(), request_storage, RequiredCap }
				end,
				[], [{node()} | ets:tab2list(?NODE_TAB)]),
	%% receive
	lists:foldl(fun({Node}, {_PrefNode, PrefFill} = Acc) ->
						receive
							{ ok, PercentFill } ->
								case PercentFill < PrefFill of
									true -> { Node, PercentFill };
									_ -> Acc
								end;
							{ error, _ } -> Acc
						after ?TIMEOUT -> Acc
						end
				end,
				{ undefined, undefined }, [{node()} | ets:tab2list(?NODE_TAB)]).


%% @doc Grabs user files from each remote node
scan(#request{action=list, broadcast=false} = Req) ->
	lists:foldl(fun({Node}, Acc) ->
					  { ?STORAGE_PROC, Node } ! { self(), Req },
					  receive
						  { ok, RemList } -> RemList++Acc;
						  { error, _ } -> Acc
					  after ?TIMEOUT -> Acc
					  end
			  end, [], ets:tab2list(?NODE_TAB)).

%% ====================================================================
%% Internal functions
%% ====================================================================

init(InitialNodes) ->
	
	globals:set(capacity, 200*1024*1024), %% 200 MB storage
	
	ets:new(?NODE_TAB, [named_table]),
	ets:insert(?NODE_TAB, [{Node} || Node <- InitialNodes] ),
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
			io:format("~w: all systems offlie.~n", [erlang:localtime()]),
			deinit();
		
		{ Pid, hello } ->
			ets:insert(?NODE_TAB, {node(Pid)}),
			loop();
		
		{ Pid, request_nodes } ->
			ets:insert(?NODE_TAB, {node(Pid)}),
			io:format("~w: nodes list served.~n", [erlang:localtime()]),
			Pid ! { ok, ets:tab2list(?NODE_TAB) },
			loop();
		
		{ Pid, request_storage, RequiredCap } ->
			case RequiredCap =< (globals:get(capacity)-globals:get(fill)) of
				true -> Pid ! { ok, globals:get(fill)/globals:get(capacity) };
				_ -> Pid ! { error, storage_full }
			end,
			loop();
		
		{ Pid, reserve_storage, RequiredCap } ->
			io:format("~w: reserving on system!~n", [erlang:localtime()]),
			globals:set(fill, globals:get(fill)+RequiredCap),
			io:format("~w: reserved, responding...~n", [erlang:localtime()]),
			Pid ! { ok, reserved },
			loop();
		
		_Other ->
			io:format("~w: system got: ~w~n", [erlang:localtime(), _Other]),
			loop()
	end.
