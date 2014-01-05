%% @author Michal
%% @doc 


-module(storage_client).
-include("shared.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
-export([create/3,
		 delete/3,
		 read/3,
		 write/4,
		 grab_ids/2,
		 send_request/2]).

%%
%% @TODO handle operations concurrently !!!
%%

create(Node, FileName, UserId) ->
	case file:read_file(FileName) of
		{ ok, RawData } ->
	send_request(Node, #request{action	= create,
								user_id	= UserId,
								options	= #create_opts{v_path	= FileName,
													   data 	= RawData}
								});
		{ error, _ } ->
			io:format("error!~n")
	end.

delete(Node, FileId, UserId) ->
	send_request(Node, #request{action	= delete,
								user_id	= UserId,
								file_id	= FileId
							   }).

read(Node, FileId, UserId) ->
	send_request(Node, #request{action	= read,
								user_id	= UserId,
								file_id	= FileId
								}).

write(Node, FileId, FileName, UserId) ->
	case file:read_file(FileName) of
		{ ok, RawData } ->
	send_request(Node, #request{action	= write,
								user_id	= UserId,
								file_id = FileId,
								options	= #write_opts{v_path	= FileName,
													  data 		= RawData}
								});
		{ error, _ } ->
			io:format("error!~n")
	end.

grab_ids(Node, UserId) ->
	send_request(Node, #request{action	= list,
								user_id	= UserId}).
	

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
