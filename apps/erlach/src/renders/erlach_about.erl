-module(erlach_about).
-author('Andy').
-compile(export_all).

-include("erlach.hrl").

title(#st{}) -> <<"About – Erlach"/utf8>>.
urn() -> <<"about">>.

                
subscr() -> [
    {?TR(<<"Постов: "/utf8>>,<<"Posts: "/utf8>>,<<"Постів: "/utf8>>),       {post,total},        fun wf:to_binary/1 },
    {?TR(<<"Тредов: "/utf8>>,<<"Threads: "/utf8>>,<<"Тредів: "/utf8>>),      {thread,total},      fun wf:to_binary/1 },
    {?TR(<<"Изображений: "/utf8>>,<<"Images: "/utf8>>,<<"Зображень: "/utf8>>), {attachment,total},  fun wf:to_binary/1 },
    {?TR(<<"Просмотров: "/utf8>>,<<"Views: ">>,<<"Переглядів"/utf8>>),        {view,total},        fun wf:to_binary/1 },
    {?TR(<<"Чтение: "/utf8>>,<<"Reading: ">>,<<"Читання: "/utf8>>),          {time,online,total}, fun(C) ->
        [wf:to_binary(trunc(C/3600)),?TR(<<" ч"/utf8>>,<<" h"/utf8>>,<<" г"/utf8>>)] end},
    {?TR(<<"Пул соединений: "/utf8>>,<<"Connections pool: "/utf8>>,<<"Пул абонентів: "/utf8>>),   {user,online,total}, fun wf:to_binary/1 }
    ].

init(#route{}=Route) ->
    wf:info(?M,"init ~p",[self()]),
    [ wf:reg({anal,Tag}) || {_,Tag,_} <- subscr() ],
    {ok,#st{user=eauth_user:get(),route=Route,action=view}}.
finalize(#st{}) -> wf:info(?M,"finalize ~p",[self()]).
terminate() ->
    [ wf:unreg({anal,Tag}) || {_,Tag,_} <- subscr() ],
    wf:info(?M,"terminate ~p",[self()]).

render(content=Panel,#st{}=S) ->
    wf:info(?M,"About",[]),
    #panel{id=Panel,body=[
        #panel{class= <<"image-logo">>},
        #span{class= <<"remark">>,body=[?TR(
            <<"Полностью анонимная имиджборда и сервис анонимных комментариев"/utf8>>,
            <<"Fully anonymous imageboard and anonymous comments service"/utf8>>,
            <<"Повністю анонімна імаджборда та сервіс анонімних коментарів"/utf8>>), ", 2015-2017"] },
        #panel{class= <<"center">>,body=[
            #panel{class= <<"erlach-version">>, body=[
                ?TR(<<"Версия"/utf8>>,<<"Version"/utf8>>,<<"Версія"/utf8>>),
                ": <strong>",?ERLACH_VERSION,"</strong>"] },
            #panel{class= <<"erlach-version">>,body= [?TR(<<"Пожертвования (BTC): "/utf8>>,
                <<"Donate (BTC): ">>,<<"Пожертвування (BTC): "/utf8>>),
                <<"<strong>1C2tHeg7e8ZsSGmaXDdykrHVoqQfjAiL1n</strong>">>]},
            #br{},
            #panel{class= <<"erlach-version">>, body=[
                "TOR: <strong>",
                #a{class= <<"erlach-feedback">>, target="_blank",
                    href="http://erlach6sjul42c7h.onion/",body= <<"erlach6sjul42c7h.onion">>},
                "</strong>"] },
            #panel{class= <<"erlach-version">>, body=[
                ?TR(<<"Обратная связь"/utf8>>,<<"Feedback"/utf8>>,<<"Зворотній зв\\'язок"/utf8>>),
                ": <strong>",
                #a{class= <<"erlach-feedback">>, target="_blank",
                    href="https://twitter.com/erlach_co",body= <<"twitter.com/erlach_co">>},
                "</strong>"] },
            
            render(statistics,S)
        ]}
    ]};
render(statistics=Class,#st{}=S) ->
    Data=[ #panel{body=[Desc,#span{id=spa:id({?M,Tag}), body=Fun(erlach_stat:count(Tag))}]} || {Desc,Tag,Fun}
            <- subscr() ],
    #panel{class=Class,body=Data}.
event(#pubsub{target=anal,action=update,element=Tag,data=D,from=F}) ->
    case lists:keyfind(Tag, 2, subscr()) of
        {_,_,Fun} ->
            P=spa:id({?M,Tag}),
            Body=case D of {Count,New} -> Fun(New); _ -> Fun(erlach_stat:count(Tag)) end,
            wf:update(P,#span{id=P,body=Body});
        false -> skip
    end;
event(Unknown) -> ?EVENT_ROUTER:unknown(?M,Unknown).
