%% @author Michal
%% @doc Shared data structures


-define(STORAGE_PROC, storage).
-define(SYSTEM_PROC, system).
-define(TIMEOUT, 1000).

-record(file, {
					id,				% string (uuid)
					name,			% string
					size,			% in bytes
					last_access,	% calendar:universal_time()
					owner_id,		% string (uuid)
					v_path	= "/"	% string
					}).

-record(request, {
				  	file_id,	% string (name in create request)
					user_id,	% string
					action,		% atom read | write | create | delete
					options		% reserved for future use
					}).
