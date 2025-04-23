-include_lib("kvs/include/kvs.hrl").
-include("db.hrl").

-record(thread, {?DB_ELEMENT(feed),
  head_post,
  anonymous,
  request_to,
  last_post_date,		    % timestamp
  publishing_interval,  % {start_timestamp|infinity, end_timestamp|infinity} | undefined
  order % top
  }).
