%% @author Michal
%% @doc Data Storage node


-module(node).
-include("shared.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
-export([start/0, start/1, stop/0]).

start() ->
	system:start(),
	storage:start().

start(InitialNode) ->
	system:start(InitialNode),
	storage:start().

stop() ->
	storage:stop(),
	system:stop().



%% ====================================================================
%% Internal functions
%% ====================================================================


