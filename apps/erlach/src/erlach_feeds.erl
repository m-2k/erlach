-module(erlach_feeds).
-author('Andy').
-export([append/1, append/3, update/2, delete/1]).
-include("erlach.hrl").

append(E) -> append(E,true,true).

append(#post{type=post,feed_id={post,Tid},sage=Sage}=P,Bump,UpdateRecent) when Sage =:= false andalso Bump =:= true ->
    R=kvs_feeds:append(P),
    {ok,T}=kvs:get(post,Tid),
    kvs_feeds:relink(T),
    case UpdateRecent of true -> update_recent(Tid); _ -> ok end,
    R;
append(#post{type=thread,id=Tid}=T,_,UpdateRecent) -> % thread always update recent
    case UpdateRecent of true -> update_recent(Tid); _ -> ok end,
    kvs_feeds:append(T);
append(E,_,_) ->
    kvs_feeds:append(E).

update(R,Fun) ->
    kvs_feeds:update(R,Fun).

delete(#post{type=thread}=T) ->
    kvs_feeds:purge(T,fun() -> [{feed,post,fun delete/1}] end);
delete(#post{}=P) ->
    delete_post(P);
delete(#attachment{name=N,path=P,info=I}=A) ->
    kvs_feeds:delete(A),
    case I of
        ?UNDEF ->
            skip;
        _ ->
            Path=fun(End) -> filename:join([erlach_thread:storage(),P,[N,End]]) end,
            [ file:delete(Path(End)) || End <- [erlach_image:ext(I),<<"-R512.jpg">>,<<"-R256.jpg">>,<<"-R128.jpg">>]]
    end.


% Private funs
update_recent(Tid) ->
    Fun=fun(#statistic{value=List}=Stc) ->
        {New,_}=spa_utils:ensure_split(wf:config(erlach,threads_recent_activity_count,100),[Tid|lists:delete(Tid,List)]),
        {ok,Stc#statistic{value=New}}
    end,
    kvs_feeds:update(#statistic{id={threads,recent_activity}},Fun).
delete_post(#post{image=I}=P) ->
    kvs_feeds:delete(P),
    is_integer(I) andalso case kvs:get(attachment,I) of {ok,A} -> delete(A); _ -> skip end.
