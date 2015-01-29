-module(db_attachment).
% -include_lib("kvs/include/kvs.hrl").
-include_lib("kvs/include/metainfo.hrl").
% -include_lib("kvs/include/feed.hrl").
% -include_lib("db/include/db.hrl").
-include_lib("db/include/attachment.hrl").
-compile(export_all).

%% rr(kvs), rr("apps/db/include/attachment.hrl"), rr("deps/kvs/include/feed.hrl").

metainfo() ->
    #schema{name=kvs,tables=[
        #table{name=attachment,container=feed,fields=record_info(fields,attachment),keys=[]}
        ]}.