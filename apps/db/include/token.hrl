-include_lib("kvs/include/kvs.hrl").

-record(token, {
  id :: {atom(), any()}, % {twitter, <<"0000000">>}
  user,
  created,
  future = []
  }).
