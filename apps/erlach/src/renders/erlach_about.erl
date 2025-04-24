-module(erlach_about).
-author('Andy').
-compile(export_all).

-include("erlach.hrl").

title(#st{}) -> <<"About – Erlach"/utf8>>.
urn() -> ?UNDEF.

subscr() -> [
    {<<"Posts: ">>,             {post,total},        fun wf:to_binary/1 },
    {<<"Threads: ">>,           {thread,total},      fun wf:to_binary/1 },
    {<<"Images: ">>,            {attachment,total},  fun wf:to_binary/1 },
    {<<"Views: ">>,             {view,total},        fun wf:to_binary/1 },
    {<<"Reading: ">>, {time,online,total}, fun(C) -> wf:f("~b h",[trunc(C/3600)]) end}
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
        #panel{class= <<"content-title">>,body= <<"About Erlach">>},
        #span{class= <<"remark">>,body= <<"Erlach Imageboard Services, 2015-2016">>},
        #panel{class= <<"center">>,body=[
            #panel{class= <<"erlach-version">>, body= <<"Version: <strong>",(?ERLACH_VERSION)/binary,"</strong>">>},
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
