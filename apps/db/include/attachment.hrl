-include_lib("kvs/include/kvs.hrl").
-include("db.hrl").

% -define(CONTAINER, id, top=undefined, entries_count=0).
% -define(ITERATOR(Container, Guard), id, container=Container, feed_id, prev, next, feeds=[], guard=Guard, etc).

% id: integer
% feed: {post, PostId}
% name: original name of file
% mime: mime type
-record(attachment, {?DB_ELEMENT(feed),
	path,
	uuid,
	description,
	mime,
	position,
	size,
	date
	}).

