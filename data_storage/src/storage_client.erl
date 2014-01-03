%% @author Michal
%% @doc 


-module(storage_client).
-include("shared.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
-export([create/2, delete/2, send_request/2]).

create(Node, FileName) ->
	send_request(Node, #request{action=create,
								file_id=FileName,
								user_id="usr123"}).

delete(Node, FileId) ->
	send_request(Node, #request{action=delete,
								file_id=FileId}).

send_request(Node, #request{} = Req) ->
	{ ?STORAGE_PROC, Node } ! { self(), Req },
	receive
		{ ok, Result } -> Result;
		{ error, Err } -> Err
	after 5000 ->
			{ error, timeout }
	end.

%% ====================================================================
%% Internal functions
%% ====================================================================
