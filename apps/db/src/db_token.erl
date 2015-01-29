-module(db_token).
% -include_lib("kvs/include/kvs.hrl").
-include_lib("kvs/include/metainfo.hrl").
% -include_lib("kvs/include/feed.hrl").
% -include_lib("db/include/db.hrl").
-include_lib("db/include/token.hrl").
-compile(export_all).

%% rr(kvs), rr("apps/db/include/thread.hrl").
%% {ok, P} = kvs:get(post, 1), kvs:put(P#post{message="AAA"}).

metainfo() ->
    #schema{name=kvs,tables=[
        #table{name=token,fields=record_info(fields,token),keys=[]}
        ]}.