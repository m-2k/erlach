-ifndef(DB_HRL).
-define(DB_HRL, "erlach_db.hrl").

-include_lib("kvs/include/kvs.hrl").
-include_lib("kvs/include/feed.hrl").

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
-record(post, {?DB_ELEMENT(feed), first, name_escaped = <<>>, message = <<>>, message_escaped = <<>>, links = [], sage = false, image }).
-record(attachment, {?DB_ELEMENT(feed),
	path,
    target,
    original_path,
	original_mime,
	size,
    hash,
	date,
    info,
    width,
    height
	}).

-endif.
