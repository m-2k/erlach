-module(db_post).
-include_lib("kvs/include/metainfo.hrl").
-include_lib("db/include/post.hrl").
-compile(export_all).

%% rr(kvs), rr("apps/db/include/post.hrl").
%% {ok, P} = kvs:get(post, 1), kvs:put(P#post{message="AAA"}).

metainfo() ->
  #schema{name=kvs,tables=[
    #table{name=post,container=feed,fields=record_info(fields,post),keys=[]}
    ]}.