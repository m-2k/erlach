-module(erlach_feeds).
-author('Andy').
-compile(export_all).
-include("erlach.hrl").
-include_lib("n2o/include/wf.hrl").

-define(CLASS,feeds).

reply(R,S) -> {reply,R,S,wf:config(feed_server,process_timeout,10*60*1000)}.
name(R) -> {element(#iterator.container,R),element(#iterator.feed_id,R)}.

call(Cmd,Record) -> call(Cmd,Record,?UNDEF).
call(Cmd,Record,Fun) ->
    wf:info(?M,"add ~p",[Record]),
    Pid=ensure_start(name(Record)),
    n2o_async:send(Pid,case Fun of F when is_function(F) -> {Cmd,Record,F}; _ -> {Cmd,Record} end).
    
ensure_start(Name) ->
    wf:info(?M,"ensure start ~p",[Name]),
    case n2o_async:pid({?CLASS,Name}) of Pid when is_pid(Pid) -> Pid; _ -> start(Name) end.

start(Name) ->
    wf:info(?M,"start ~p ~p",[self(),Name]),
    {Pid,Name}=n2o_async:start(#handler{module=?M,group=?APP,class=?CLASS,name=Name,state=[]}),
    Pid.

stop(Name) ->
    wf:info(?M,"stop: ~p ~p",[self(),Name]),
    n2o_async:stop(?CLASS,Name).

proc(init,H) ->
    wf:info(?MODULE,"proc init: ~p ~p",[self(),H]),
    {ok,H};
proc(timeout,#handler{}=H) ->
    wf:info(?M,"proc timeout ~p",[self()]),
    case wf:config(feed_server,process_timeout_action,stop) of
        stop -> {stop,normal,H};
        hibernate -> {noreply,H,hibernate}
    end;
proc(M,#handler{}=H) ->
    wf:info(?M,"proc exec: ~p ~p ~p",[self(),M,H]),
    reply(exec(M),H).

path(P) -> filename:join(wf:config(n2o,upload,code:priv_dir(erlach)),wf:to_list(P)).

exec({append,#post{type=post,feed_id={post,Tid},sage=Sage}=P}) ->
    R=kvs:add(P),
    case Sage of true -> skip; false -> {ok,T}=kvs:get(post,Tid), {ok,T2}=call(relink,T) end,
    R;
exec({append,R}) -> kvs:add(R);
exec({update,R,Fun}) ->
    case kvs:get(element(1,R),element(2,R)) of
        {ok,R2} -> case Fun(R2) of {ok,R3} -> kvs:put(R3), {ok,R3}; Skip -> Skip end;
        E -> E
    end;
exec({relink,R}) ->
    {ok,F}=kvs:get(element(#iterator.container,R),element(#iterator.feed_id,R)),
    kvs:relink(F,R,#kvs{mod=?DBA}),
    kvs:link(R);
exec({delete,#post{type=post,id=Id,image=?UNDEF}=P}) ->
    kvs:remove(post,Id);
exec({delete,#post{type=post,id=Id,image=Im}=P}) ->
    kvs:remove(post,Id),
    case kvs:get(attachment,Im) of {ok,A} -> call(delete,A); _ -> skip end;
exec({delete,#post{type=thread,id=Tid}=T}) ->
    exec({delete,T#post{type=post}}), % remove thread & image
    case kvs:get(feed,{post,Tid}) of
        {ok, #feed{top=Top,count=Count}} -> % delete -- raw ops
            kvs:fold(fun(#post{id=Pid},Acc) -> kvs:delete(post,Pid), Acc end,[],post,Top,Count,#iterator.prev,#kvs{mod=?DBA}),
            kvs:delete(feed,{post,Tid});
        _ -> skip
    end;
exec({delete,#attachment{id=Id,path=P}}) ->
    kvs:remove(attachment,Id),
    file:delete(path(P));
exec({delete,R}) ->
    kvs:remove(element(1,R),element(2,R));
exec(_) -> skip.