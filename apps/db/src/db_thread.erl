-module(db_thread).
-include_lib("kvs/include/metainfo.hrl").
-include_lib("db/include/thread.hrl").
-compile(export_all).

%% rr(kvs), rr("apps/db/include/thread.hrl").
%% {ok, P} = kvs:get(post, 1), kvs:put(P#post{message="AAA"}).

metainfo() ->
  #schema{name=kvs,tables=[
    #table{name=thread,container=feed,fields=record_info(fields,thread),keys=[]}
    ]}.