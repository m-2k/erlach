-ifndef(DB_HRL).
-define(DB_HRL, "erlach_db.hrl").

-include_lib("kvs/include/kvs.hrl").
-include_lib("kvs/include/feed.hrl").

-record(statistic,  {id, value, group = noname, opts = []}).

-define(DB_ELEMENT(Container),
    ?ITERATOR(Container),
	created,
    deleted = false :: boolean(),
	type,
    readonly = false :: boolean(),
	hidden = false :: boolean(),
    name = <<>> :: binary(),
    desc = <<>> :: binary(),
    urn = <<>> :: binary(),
	view = [] :: list(),
	tags = [] :: list()
    ).

-record(db_element,  {?DB_ELEMENT(undefined)}).
-record(party, {?DB_ELEMENT(feed)}).
-record(board, {?DB_ELEMENT(feed),
    limit % bump_limit
    }).

-record(post, {?DB_ELEMENT(feed),
    party,
    board,
    thread,
    name_escaped = <<>>,
    message = <<>>,
    message_escaped = <<>>,
    links = [],
    sage = false,
    image,
    nickname
    }).

-record(attachment, {?DB_ELEMENT(feed),
    party,
    board,
    thread,
    post,
	path,
    hash,
    width,
    height,
	original_info,
    info
	}).

-endif.
