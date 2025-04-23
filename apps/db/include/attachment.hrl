-include_lib("kvs/include/kvs.hrl").
-include("db.hrl").

% id: integer
% feed: {post, PostId}
% name: original name of file
% mime: mime type
-record(attachment, {?DB_ELEMENT(feed),
  path,
  uuid,
  description,
  mime,
  position,
  size,
  date
  }).

