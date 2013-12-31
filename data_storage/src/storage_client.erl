%% @author Michal
%% @doc 


-module(storage_client).

%% ====================================================================
%% API functions
%% ====================================================================
-export([add/2, list/1]).

add(Node, File) ->
	{ storage, Node } ! { self(), add, File },
	receive
		{ ok, Result } ->
			Result;
		{ error, Result } ->
			Result
	end.

list(Node) ->
	{ storage, Node } ! { self(), list },
	receive
		{ ok, List } ->
			List;
		{ error, Result } ->
			Result
	end.


%% ====================================================================
%% Internal functions
%% ====================================================================


