-module(erlach_feeds).
-author('Andy').
-export([append/1, update/2, delete/1]).
-include("erlach.hrl").


append(#post{type=post,feed_id={post,Tid},sage=false}=P) ->
    R=spa_feeds:call(append,P),
    {ok,T}=kvs:get(post,Tid),
    spa_feeds:call(relink,T),
    update_recent(Tid),
    R;
append(#post{type=thread,id=Tid}=T) ->
    update_recent(Tid),
    spa_feeds:call(append,T);
append(R) ->
    spa_feeds:call(append,R).

update(R,Fun) ->
    spa_feeds:call(update,R,Fun).

delete(#post{type=thread}=T) ->
    spa_feeds:call(purge,T,fun() -> [{feed,post,fun delete/1}] end);
delete(#post{}=P) ->
    delete_post(P);
delete(#attachment{info=I}=A) ->
    spa_feeds:call(delete,A),
    case I of ?UNDEF -> skip; _ -> file:delete(erlach_thread:path(A)) end.


% Private funs
update_recent(Tid) ->
    Fun=fun(#statistic{value=List}=Stc) ->
        {New,_}=spa_utils:ensure_split(wf:config(erlach,threads_recent_activity_count,100),[Tid|lists:delete(Tid,List)]),
        {ok,Stc#statistic{value=New}}
    end,
    spa_feeds:call(update,#statistic{id={threads,recent_activity}},Fun).
delete_post(#post{image=I}=P) ->
    spa_feeds:call(delete,P),
    is_integer(I) andalso case kvs:get(attachment,I) of {ok,A} -> delete(A); _ -> skip end.
