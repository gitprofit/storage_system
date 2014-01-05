%% @author Michal
%% @doc 


-module(storage_client).
-include("shared.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
-export([create/2,
		 delete/2,
		 read/2,
		 write/3,
		 send_request/2]).

create(Node, FileName) ->
	case file:read_file(FileName) of
		{ ok, RawData } ->
	send_request(Node, #request{action	= create,
								user_id	= "usr123",
								options	= #create_opts{v_path	= FileName,
													   data 	= RawData}
								});
		{ error, _ } ->
			io:format("error!~n")
	end.

delete(Node, FileId) ->
	send_request(Node, #request{action	= delete,
								user_id	= "usr123",
								file_id	= FileId
							   }).

read(Node, FileId) ->
	send_request(Node, #request{action	= read,
								user_id	= "usr123",
								file_id	= FileId
								}).

write(Node, FileId, FileName) ->
	case file:read_file(FileName) of
		{ ok, RawData } ->
	send_request(Node, #request{action	= write,
								user_id	= "usr123",
								file_id = FileId,
								options	= #write_opts{v_path	= FileName,
													  data 		= RawData}
								});
		{ error, _ } ->
			io:format("error!~n")
	end.
	

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
