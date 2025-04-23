-module(static).
-author('andy').
-vsn('0.0.0').

-compile(export_all).
-include_lib("n2o/include/wf.hrl").
-include_lib("erlach_db/include/erlach_db.hrl").
-include_lib("nitro/include/nitro.hrl").
-include("erlach.hrl").

-define(PRIVACY_POST_ID, 1).
-define(PRIVACY_THREAD_ID, 1).
-define(PRIVACY_BOARD_URI, e).

main() -> #dtl{file="erlach",app=erlach,bindings=[{body,body()}, {title,<<"Privacy">>}]}.
body() ->
    
    Pid = case ?CTX#cx.path#route.option of
        privacy -> 1;
        terms -> 2;
        about -> 3 end,
    
    Content = case kvs:get(post, Pid) of
        {ok, #post{feed_id={post,Tid}}=Post} ->
            case kvs:get(thread, Tid) of
                {ok, #thread{name=Topic,feed_id={thread,Bid}}=Thread} ->
                    {ok, Html} = html:post(Thread,Post,u:is_admin(),u:id(),[]),
                    [ #panel{class= <<"content-title">>,body=Topic},
                        #panel{id=posts,body=[Html]},
                        #panel{class= <<"center">>,body=[
                            #link{class= <<"button primary">>, href=erlach_qs:ml({thread,blog,erlach_qs:board_id_to_uri(Bid),Tid}),
                                body= <<"View thread">>} ]} ];
                _ -> [] end;
        _ -> []
    end,
    html:body(Content).

event(init) -> ok;

event(terminate) -> skip;
event(Event) -> guard:shared_event(?MODULE, Event).
