-include_lib("kvs/include/kvs.hrl").
-include("db.hrl").


% -define(CONTAINER, id, top=undefined, entries_count=0).
% -define(ITERATOR(Container, Guard), id, container=Container, feed_id, prev, next, feeds=[], guard=Guard, etc).


%% feed_id :: {board, integer()}
-record(thread, {?DB_ELEMENT(feed),
    head_post,
	anonymous,
	request_to,
	last_post_date,			% timestamp
	publishing_interval,	% {start_timestamp|infinity, end_timestamp|infinity} | undefined
    order % top
    }).
