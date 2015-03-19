-include_lib("kvs/include/kvs.hrl").
-include("db.hrl").

-record(group, {?DB_ELEMENT(feed), anonymous, description }).
