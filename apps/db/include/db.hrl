-ifndef(JOURNAL_HRL).
-define(JOURNAL_HRL, "db.hrl"). % prevent ../apps/db/include/db.hrl:3: redefining macro 'ELEMENT'

-include_lib("kvs/include/kvs.hrl").

-define(ELEMENT,
	created,		% timestamp
	published,		% timestamp
	type = default,			% thread::default|blog|request, post::default|question
	access,			% undefined (none)|all|registered|private (access check)|charge (payment)|promo (title & thumb image)
	request_thread, % thread id
	hidden,			% undefined|any(), user defined
	deleted,		% undefined|any(), modefator defined
	draft,
	name,
	view = [],
	count,			% count of entry elements
	temporary = true :: false|true,		% machine logic for pre-storing
	tags = [],		% tag names
	future = []).		% extends fields for future

% -record(element,  {?ELEMENT}).

% -define(LOG_HEADER, game_id, date, time, user, module, type, speed, rounds).
% -define(CONTAINER_LOG, ?CONTAINER, ?LOG_HEADER, stats=[]).
% -record(container_log, {?CONTAINER_LOG}).
% -record(container_event, {?ITERATOR(container_log), ?LOG_HEADER }).

-endif.