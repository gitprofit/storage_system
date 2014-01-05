%% @author Michal
%% @doc Shared data structures


-define(STORAGE_PROC, storage).
-define(SYSTEM_PROC, system).
-define(TIMEOUT, 1000).

-record(file, {
					id,					% string (uuid)
					size,				% in bytes
					last_access,		% calendar:universal_time()
					owner_id,			% string (uuid)
					v_path	= "/empty"	% string
					}).

-record(request, {
				  	file_id,			% string (name in create request)
					user_id,			% string
					action,				% atom read | write | create | delete | move | list
					broadcast = true,	% atom true | false
					options				% request-specific record
					}).

-record(create_opts, {
						v_path = "/empty",	% string
					  	force_loc = false,	% atom true | false
						data				% raw data
						}).

-record(write_opts, {
						v_path = no_upd,	% undefined = no update
						data = no_upd		%
						}).

-record(move_opts, {
						new_loc		% atom (node)
  						}).
