-ifndef(DB_HRL).
-define(DB_HRL, "db.hrl"). % prevent ../apps/db/include/db.hrl:3: redefining macro 'ELEMENT'

-include_lib("kvs/include/kvs.hrl").

-define(DB_ELEMENT(Container),
    ?ITERATOR(Container),
	created,		% timestamp
    temporary = true :: false|true,		% machine logic for pre-storing
	published,		% timestamp
	type,	% thread::default|blog|request, post::default|question
	access = [] :: list(tuple()),
	request_thread, % thread id
	hidden,			% undefined|any(), user defined
	deleted,		% undefined|any(), modefator defined
	draft,
    user,
    user_name,
    name,
	view = [] :: list(),
	count,			% count of entry elements
    links = [],
	tags = [] :: list(),		% tag names
	future = []).		% extends fields for future

% -record(element,  {?ELEMENT}).

% -define(LOG_HEADER, game_id, date, time, user, module, type, speed, rounds).
% -define(CONTAINER_LOG, ?CONTAINER, ?LOG_HEADER, stats=[]).
% -record(container_log, {?CONTAINER_LOG}).
% -record(container_event, {?ITERATOR(container_log), ?LOG_HEADER }).

-record(db_element,  {?DB_ELEMENT(undefined)}).

-endif.
%
%
% #element.access :: [undefined|registered|{payment (Group), board (Level), write (Action), blog (Type)}]
%
% #access :: {{user, Uid}, {Gpoup, Level, Id}} = [ {Action,Type, infinity (Begins), infinity (Expire)} ]
%
