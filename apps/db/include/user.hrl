-include_lib("kvs/include/kvs.hrl").
-include_lib("db.hrl").

% -define(CONTAINER, id, top=undefined, entries_count=0).
% -define(ITERATOR(Container, Guard), id, container=Container, feed_id, prev, next, feeds=[], guard=Guard, etc).

-record(name, {?ITERATOR(feed),
    % id,
	name,		% not use
	displayname,% not use
	created,	% timestamp
    banned,		% infinity | {expired, date()}
	deleted,
    role, 		% = user :: administrator | moderator | user,
    birthday,
    sex,
    photo,		% attachment id
	future = []
    }).

% #access:
% {feature, admin},
% {read, blog, Id}, {write, board, Id}
% {moderate, board, Id}
% {moderate, users} % ban

-record(user3, {?ITERATOR(feed), ?ELEMENT,
	balance,
    expired,
    enabled,
    password,	% bcript
    salt,		% random
    tokens,		% [{n2o, <<"sid1">>},{n2o, <<"sid2">>},{twitter, <<"key">>}]
	last_post_date, % preventing DoS
    % names = [] :: [#name{}],
    % date,
    % zone,
    email,
    phone,
	timezone,
	lang
    }).
