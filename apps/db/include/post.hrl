-include_lib("kvs/include/kvs.hrl").
-include("db.hrl").

-record(post, {?DB_ELEMENT(feed),
  head, % true | undefined
  message,
  netw_info, % ip or more
  markup = undefined :: plaintext | markdown | undefined
  }).
