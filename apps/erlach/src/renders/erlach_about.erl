-module(erlach_about).
-author('Andy').
-compile(export_all).

-include("erlach.hrl").

title(#st{}) -> <<"About – Erlach"/utf8>>.
urn() -> ?UNDEF.

subscr() -> [
    {<<"Постов: "/utf8>>,       {post,total},        fun wf:to_binary/1 },
    {<<"Тредов: "/utf8>>,       {thread,total},      fun wf:to_binary/1 },
    {<<"Изображений: "/utf8>>,  {attachment,total},  fun wf:to_binary/1 },
    {<<"Просмотров: "/utf8>>,   {view,total},        fun wf:to_binary/1 },
    {<<"Чтение: "/utf8>>,       {time,online,total}, fun(C) -> [wf:to_binary(trunc(C/3600)),<<" ч"/utf8>>] end}
    ].

init(#route{}=Route) ->
    wf:info(?M,"init ~p",[self()]),
    [ wf:reg({anal,Tag}) || {_,Tag,_} <- subscr() ],
    {ok,#st{route=Route,action=view}}.
finalize(#st{}) -> wf:info(?M,"finalize ~p",[self()]).
terminate() ->
    [ wf:unreg({anal,Tag}) || {_,Tag,_} <- subscr() ],
    wf:info(?M,"terminate ~p",[self()]).

render(content=Panel,#st{}=S) ->
    wf:info(?M,"About",[]),
    #panel{id=Panel,body=[
        #panel{class= <<"content-title">>,body= <<"Эрлач"/utf8>>},
        #span{class= <<"remark">>,body=
            <<"Полностью анонимная имиджборда и сервис анонимных комментариев, 2015-2016"/utf8>>},
        #panel{class= <<"center">>,body=[
            #panel{class= <<"erlach-version">>, body= <<"Версия: <strong>"/utf8,(?ERLACH_VERSION)/binary,"</strong>"/utf8>>},
            #panel{class= <<"erlach-feedback">>, body= <<"&#116;&#119;&#105;&#116;&#64;erlach&#46;co">>},
            render(statistics,S)
        ]}
    ]};
render(statistics=Class,#st{}=S) ->
    Data=[ #panel{body=[Desc,#span{id=spa:id({?M,Tag}), body=Fun(erlach_stat:count(Tag))}]} || {Desc,Tag,Fun} <- subscr() ],
    #panel{class=Class,body=Data}.
event(#pubsub{target=anal,action=update,element=Tag,data={Count,New},from=F}) ->
    case lists:keyfind(Tag, 2, subscr()) of
        {_,_,Fun} -> P=spa:id({?M,Tag}), wf:update(P,#span{id=P,body=Fun(New)});
        false -> skip
    end;
event(Unknown) -> ?EVENT_ROUTER:unknown(?M,Unknown).
