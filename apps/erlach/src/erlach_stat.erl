-module(erlach_stat).
-author('Andy').
-compile(export_all).
-include("erlach.hrl").


count(Thing) -> case kvs:get(id_seq,{anal,Thing}) of {ok,#id_seq{id=Count}} -> Count; _ -> 0 end.

do_incr(Thing) -> do_incr(Thing,1).
do_incr(Thing,Count) ->
    New=kvs:next_id({anal,Thing},Count),
    wf:send({anal,Thing},{server,#pubsub{target=anal,action=update,element=Thing,data={Count,New},from=self()}}).

incr(Type,Meta) -> incr(Type,1,Meta).
decr(Type,Meta) -> incr(Type,-1,Meta).

incr(attachment,Count,{Bid,Tid,#attachment{}}) ->
    do_incr({attachment,total},Count),
    do_incr({attachment,thread,Tid},Count),
    do_incr({attachment,board,Bid},Count);
incr(thread,Count,{Bid,#post{type=thread}}) ->
    do_incr({thread,total},Count),
    do_incr({thread,Bid},Count);
incr(post,Count,{Bid,Tid,#post{type=post,sage=Sage}}) ->
    do_incr({post,total},Count),
    do_incr({post,thread,Tid},Count),
    do_incr({post,board,Bid},Count),
    case Sage of
        true ->
            do_incr({post,sage,total},Count),
            do_incr({post,sage,thread,Tid},Count),
            do_incr({post,sage,board,Bid},Count);
        false -> skip
    end.

ts() -> erlang:monotonic_time(seconds).
time_start(#st{board=Bd,thread=Th}=S) ->
    erlang:put({?M,time,statistic},ts()),
    do_incr({view,total}),
    case {Bd,Th} of
        {#board{},#post{id=Tid}} -> do_incr({view,thread,Tid});
        {#board{id=Bid},_} -> do_incr({view,board,Bid});
        _ -> do_incr({view,static,erlach_qs:state_to_render(S)})
    end.
time_stop(#st{board=Bd,thread=Th}=S) ->
    case erlang:erase({?M,time,statistic}) of
        T when is_integer(T) ->
            Td = ts() - T,
            do_incr({time,online,total},Td),
            case {Bd,Th} of
                {#board{id=Bid},#post{id=Tid}} ->
                    do_incr({time,online,board,Bid},Td),
                    do_incr({time,online,thread,Tid},Td);
                {#board{id=Bid},_} ->
                    do_incr({time,online,board,Bid},Td);
                _ ->
                    do_incr({time,online,static,erlach_qs:state_to_render(S)},Td)
            end;
        _ -> wf:warning(?M,"Timer not started",[])
    end.