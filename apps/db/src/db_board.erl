-module(db_board).
-include_lib("kvs/include/metainfo.hrl").
-include_lib("db/include/board.hrl").
-compile(export_all).

%% rr(kvs), rr("apps/db/include/thread.hrl").
%% {ok, P} = kvs:get(post, 1), kvs:put(P#post{message="AAA"}).

metainfo() ->
    #schema{name=kvs,tables=[
        #table{name=board,container=feed,fields=record_info(fields,board),keys=[]}
        ]}.

new(Uri,Name,Description,GroupID,Categories,Hidden,Access) ->
    kvs:add( #board{ id=kvs:next_id(board, 1),
        created=erlang:now(),
        uri=Uri,
        name=Name,
        user=0,
        description=Description,
        category=Categories,
        hidden=Hidden,
        feed_id={board, GroupID},
        temporary=false,
        access=Access
        }).