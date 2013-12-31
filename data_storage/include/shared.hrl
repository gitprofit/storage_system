%% @author Michal
%% @doc Shared data structures

-record(metadata, {
					id,		% int
					name,	% string
					}).

-record(request, {
				  	id,			% int
					operation	% atom read | write | create | delete
					}).
