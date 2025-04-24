-module(kvs_feeds_server).
-author('Andy').
-compile(export_all).

-include("kvs_feeds.hrl").
-include_lib("n2o/include/wf.hrl").
-include_lib("kvs/include/config.hrl").
-include_lib("kvs/include/feed.hrl").

-define(GROUP,kvs_feeds). % genserver application name
-define(CLASS,feed_server).  % ets caching key prefix

reply(R,S) -> {reply,R,S,?CONFIG(feed_timeout,10*60*1000)}.
feed_channel(R) ->
    case element(2,R) of
        Id when is_integer(Id) -> {element(#iterator.container,R),element(#iterator.feed_id,R)};
        _ -> {element(1,R),element(2,R)}
    end.

log(Action,#handler{name=FeedChannel},Pid) -> log(Action,FeedChannel,Pid);
log(Action,Data,Pid) -> ?LOG(" ~p ~p ~p",[Pid,Action,Data]).

call(Cmd,Record) -> call(Cmd,Record,?UNDEF).
call(Cmd,Record,Fun) ->
    log({call,Cmd},{element(1,Record),element(2,Record)},self()),
    Pid=ensure_start(feed_channel(Record)),
    n2o_async:send(Pid,case Fun of F when is_function(F) -> {Cmd,Record,F}; _ -> {Cmd,Record} end).
    
ensure_start(FeedChannel) ->
    log(ensure_start,FeedChannel,self()),
    case n2o_async:pid({?CLASS,FeedChannel}) of Pid when is_pid(Pid) -> Pid; _ -> start(FeedChannel) end.

start(FeedChannel) ->
    H=#handler{module=?M,group=?GROUP,class=?CLASS,name=FeedChannel,state=[]},
    {Pid,FeedChannel}=n2o_async:start(H),
    log(start,H,Pid),
    Pid.

stop(FeedChannel) ->
    log(stop,FeedChannel,self()),
    n2o_async:stop(?CLASS,FeedChannel).

proc(init,H) ->
    log(init,H,self()),
    {ok,H};
proc(timeout,#handler{}=H) ->
    case ?CONFIG(feed_timeout_action,hibernate) of
        stop -> log(stop,H,self()), {stop,normal,H};
        hibernate -> log(hibernate,H,self()), {noreply,H,hibernate}
    end;
proc(M,#handler{}=H) ->
    log(exec,H,self()),
    reply(exec(M),H).

exec({append,R}) -> log(append,R,self()), kvs:add(R);
exec({update,R,Fun}) ->
    log(update,{R,Fun},self()),
    case kvs:get(element(1,R),element(2,R)) of
        {ok,R2} -> case Fun(R2) of {ok,R3} -> kvs:put(R3), {ok,R3}; Skip -> Skip end;
        E -> E
    end;
exec({eval,R,Fun}) ->
    log(eval,{R,Fun},self()),
    case kvs:get(element(1,R),element(2,R)) of
        {ok,R2} -> Fun(R2);
        E -> E
    end;
exec({relink,R}) ->
    log(relink,R,self()),
    {ok,R2}=kvs:unlink(R),
    kvs:link(R2);
exec({delete,R}) ->
    log(delete,R,self()),
    kvs:remove(element(1,R),element(2,R));
exec({purge,R,PurgeFun}) -> % with feed
    log(purge,R,self()),
    Tab=element(1,R),
    Id=element(2,R),
    kvs:remove(Tab,Id),
    
    PurgeFeed=fun({PC,PT,Fun}) ->
        case kvs:get(PC,{PT,Id}) of
            {ok,#feed{top=Top,count=Count}} ->
                kvs:delete(PC,{PT,Id}), % delete -- raw ops
                EntryList=kvs:fold(fun(Child,Acc) -> [Child|Acc] end,
                    [],PT,Top,Count,#iterator.prev),
                [ Fun(Entry) || Entry <- EntryList ];
            _ -> skip
        end
    end,
    lists:map(PurgeFeed,PurgeFun());
    
exec(C) -> log(unknown,C,self()), skip.