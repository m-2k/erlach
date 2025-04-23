-include_lib("kvs/include/kvs.hrl").
-include("db.hrl").

%% feed_id :: {group, integer()}
-record(board, {?DB_ELEMENT(feed),
  uri,
  anonymous,
  description
  }).
