-include_lib("kvs/include/kvs.hrl").

% -define(CONTAINER, id, top=undefined, entries_count=0).
% -define(ITERATOR(Container, Guard), id, container=Container, feed_id, prev, next, feeds=[], guard=Guard, etc).

-record(token, {
    id :: {atom(), any()}, % {twitter, <<"0000000">>}
    user,
    created,
	future = []
    }).
