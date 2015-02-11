-include_lib("kvs/include/kvs.hrl").
-include("db.hrl").

% -define(CONTAINER, id, top=undefined, entries_count=0).
% -define(ITERATOR(Container, Guard), id, container=Container, feed_id, prev, next, feeds=[], guard=Guard, etc).

%% feed_id :: {thread, integer()}
-record(post, {?DB_ELEMENT(feed),
	head, % true | undefined
    message,
	netw_info, % ip or more
	markup = undefined :: plaintext | markdown | undefined
    }).
