%% @author Michal
%% @doc Shared data structures


-record(file, {
					id,			% string (uuid)
					name,		% string
					size,		% in bytes
					lastAccess,	% ???
					owner_id,	% string (uuid)
					localPath	% string
					}).

-record(request, {
				  	id,			% int
					operation	% atom read | write | create | delete
					}).
