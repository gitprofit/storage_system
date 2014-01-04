%% @author Michal
%% @doc Data Storage node


-module(node).
-include("shared.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
-export([start/0, start/1, stop/0]).

start() ->
	start([node()]).

start(InitialNodes) ->
	globals:init(),
	system:start(InitialNodes),
	storage:start().

stop() ->
	storage:stop(),
	system:stop(),
	globals:deinit().



%% ====================================================================
%% Internal functions
%% ====================================================================


