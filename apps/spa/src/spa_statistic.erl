-module(spa_statistic).
-author('Andy').
-compile(export_all).
-include("spa.hrl").
-include("spa_db.hrl").

do_incr(Thing) -> do_incr(Thing,1).
do_incr(Thing,Count) ->
    New=kvs:next_id({analitics,Thing},Count),
    PubSub=?RM:new(pubsub,[
        {#pubsub.target,analitics},
        {#pubsub.action,update},
        {#pubsub.element,Thing},
        {#pubsub.data,{Count,New}}
    ]),
    wf:send({analitics,Thing},{server,PubSub}).

count(Thing) -> case kvs:get(id_seq,{analitics,Thing}) of {ok,#id_seq{id=Count}} -> Count; _ -> 0 end.

ts() -> erlang:monotonic_time(seconds).

init(Fun) ->
    erlang:put({?M,time,statistic},ts()),
    [ do_incr(X) || X <- Fun() ].

finish(Fun) ->
    case erlang:erase({?M,time,statistic}) of
        T when is_integer(T) ->
            Td = ts() - T,
            [ do_incr(X,Td) || X <- Fun() ];
        _ -> wf:warning(?M,"Timer not started",[])
    end.