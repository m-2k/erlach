-ifndef(SPA_DB_HRL).
-define(SPA_DB_HRL, true).

-include_lib("kvs/include/kvs.hrl").
-include("spa_basic.hrl").

-type timestamp() :: {non_neg_integer(),non_neg_integer(),non_neg_integer()}.

-define(DB_ELEMENT(Container),
    ?ITERATOR(Container),
	created :: timestamp(),
    deleted = false :: boolean(),
    urn = <<>> :: binary(),
    name = <<>> :: binary(),
    view = [] :: list()
    ).
-record(db_element,  {?DB_ELEMENT(undefined)}).

-endif.
