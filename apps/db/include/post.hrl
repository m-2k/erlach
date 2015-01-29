-include_lib("kvs/include/kvs.hrl").
-include("db.hrl").

% -define(CONTAINER, id, top=undefined, entries_count=0).
% -define(ITERATOR(Container, Guard), id, container=Container, feed_id, prev, next, feeds=[], guard=Guard, etc).

%% feed_id :: {thread, integer()}
-record(post, {?ITERATOR(feed), ?ELEMENT,
	head, % true | undefined
    user,
    message,
	netw_info, % ip or more
	links = [],
	markup = undefined :: plaintext | markdown | undefined
    }).
