-ifndef(FULLTEXT_SERACH_DB_HRL).
-define(FULLTEXT_SERACH_DB_HRL, "fulltext_search_db.hrl").

-include_lib("kvs/include/kvs.hrl").
-include_lib("kvs/include/feed.hrl").

-record(fulltext_index,{ ?ITERATOR(feed),
    index = <<>> :: any(),
    data = [] :: list(),
    meta = [] :: any() }).

-endif.


