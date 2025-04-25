-module(erlach_search).
-author('Andy').
-compile(export_all).

-include("erlach.hrl").

title(#st{}) -> <<"Live search – Erlach"/utf8>>.

-define(TIMEOUT,"300").
-define(CHECKBOX,"search-checkbox").
-define(TEXTBOX,"search-text").
-define(TEXTBOXA,'search-text').
-define(BUTTON,"search-go").
-define(CONTENT,"search-content").

limit() -> wf:config(erlach,search_max_results,50).

init_wire() ->
    wf:wire([
        #bind{target=?TEXTBOX,type=keyup,
            postback=["window.search_timeout && clearTimeout(window.search_timeout);",
            "window.search_timeout = window.setTimeout(function(){ qi('",?BUTTON,"').click(); }, ",?TIMEOUT,");"]},
        #bind{target=?CHECKBOX,type=change,postback=["var c=qi('",?CONTENT,"'); ",
            "if(c) { if(event.target.checked) {",
                "c.classList.add('enabled'); var t=qi('",?TEXTBOX,"'); t && t.focus();",
                "} else { c.classList.remove('enabled') }}"]},
        #event{target=?CHECKBOX,type=change,postback=#render_event{render=?M,target=search,event=onoff},source=[?CHECKBOX]}
    ]).

hide_search_content() ->
    wf:wire(["var c=qi('",?CHECKBOX,"'); c && c.checked && c.click();"]).

render('search-button'=P,#st{}) ->
    #label{for=?CHECKBOX, class=[l,selector,P],body=[
        #span{class=normal,body=wf:jse(svg_search_button(normal))}, %body= <<"⚲"/utf8>>},
        #span{class=close,body=wf:jse(svg_search_button(close))} %body= <<"✕"/utf8>>}
    ]};
render('search-go'=P,#st{}) -> #button{id=P,body="Go",postback=#render_event{render=?M,target=search,event=go},source=[?TEXTBOXA]};
render(?CONTENT,#st{}) -> #panel{id=?CONTENT}.

fold(List,Accumulator,S) ->
    Limit=limit(),
    Omitted=case length(List) > Limit of true -> omitted; false -> full end,
    List2=case Omitted of omitted -> lists:sublist(List,Limit); full -> List end,
    lists:foldl(fun({Pid,Count},{{TAcc,To},{PAcc,Po},Cache}) ->
        case kvs:get(post,Pid) of
            {ok,#post{type=thread,party=PartyId,board=Bid}=Thread} ->
                {Party,C2}=cache(party,PartyId,Cache),
                {Board,C3}=cache(board,Bid,C2),
                case erlach_board:access(Board) of
                    allow ->
                        H=#hes{party=Party,board=Board},
                        E=erlach_board:render(Thread,H,S),
                        {{[E|TAcc],Omitted},{PAcc,Po},C3};
                    _ ->
                        {{TAcc,Omitted},{PAcc,Po},C3}
                end;
            {ok,#post{type=post,party=PartyId,board=Bid,thread=Tid}=Post} ->
                {Party,C2}=cache(party,PartyId,Cache),
                {Board,C3}=cache(board,Bid,C2),
                {Thread,C4}=cache(post,Tid,C3),
                
                case erlach_board:access(Board) of
                    allow ->
                        H=#hes{party=Party,board=Board,thread=Thread},
                        H2=spa:setoption('to-thread-link',true,H),
                        E=erlach_board:render(Post,H2,S),
                        {{TAcc,To},{[E|PAcc],Omitted},C4};
                    _ ->
                        {{TAcc,To},{PAcc,Omitted},C4}
                end;
            _ ->
                {{TAcc,To},{PAcc,Po},Cache}
        end
    end,Accumulator,List2).

render(?CONTENT,#hes{option={search,Q}},#st{}=S) ->
    Res=fulltext_search:search(Q),
    {{TE,To},{PE,Po},_Cache}=lists:foldl(fun({post,  List},A) -> fold(List,A,S);
                                  ({thread,List},A) -> fold(List,A,S);
                                  (_,A) -> A
    end,{{[],full},{[],full},#{}},Res),
    
    wf:info(?M,"Cache ~p",[_Cache]),
    case {TE,PE} of
        {[],[]} -> #panel{id=?CONTENT};
        _ -> #panel{id=?CONTENT,class='enabled',body=[
                #panel{class='content-title',body=#span{class=section,body=
                    ?TR(<<"Результаты"/utf8>>,<<"Results"/utf8>>,<<"Результати"/utf8>>)}},
                case TE of [] -> [];
                    _ -> [#span{class=[subtitle,orange],body=?TR(<<"Треды"/utf8>>,<<"Threads"/utf8>>,<<"Тредi"/utf8>>)},TE] end,
                case PE of [] -> [];
                    _ -> [#span{class=[subtitle,blue],body=?TR(<<"Посты"/utf8>>,<<"Posts"/utf8>>,<<"Пости"/utf8>>)},PE] end,
                case To =:= omitted orelse Po =:= omitted of
                    true -> #span{class=remark,body=?TR(<<"Остальные результаты опущены"/utf8>>,
                        <<"Other results is omitted"/utf8>>,<<"Інші результати пропущені"/utf8>>)};
                    false -> []
                end
            ]}
    end.

cache(Table,Id,#{}=Cache) ->
    case maps:find({Table,Id},Cache) of
        {ok,E} -> {E,Cache};
        error ->
            {ok,E}=kvs:get(Table,Id),
            % case E of #post{type=post} -> {E,Cache}; _ -> {E,maps:put({Table,Id},E,Cache)} end
            {E,maps:put({Table,Id},E,Cache)}
    end.
    
search_off() -> spa:st((spa:st())#st{action=view}).
search_on(Q) -> spa:st((spa:st())#st{action=search,search=Q}).

event(#render_event{target=search,event=onoff}) ->
    wf:info(?M,"ONOFF ~p",[wf:q(?CHECKBOX)]),
    case wf:q(?CHECKBOX) of
        <<"on">> -> search_on(wf:q(?TEXTBOXA));
        _ -> search_off()
    end,
    erlach_qs:history_update();
event(#render_event{target=search,event=go}) ->
    Q=wf:q(?TEXTBOXA),
    search_on(Q),
    erlach_qs:history_update(),
    spa:update(?M,?CONTENT,#hes{option={search,Q}});
event(Unknown) -> ?EVENT_ROUTER:unknown(?M,Unknown).

index_all() ->
    {atomic,IdList}=mnesia:transaction(fun() -> mnesia:all_keys(post) end),
    erlang:put(xx,0),
    lists:foreach(fun(Id) ->
        index_post(Id),
        timer:sleep(30),
        X=erlang:get(xx)+1, erlang:put(xx,X),
        wf:info(?MODULE,"Index done ~p of ~p",[X,length(IdList)])
    end,IdList).

check_post(#post{party=P}) ->
    case kvs:get(party,P) of
        {ok,#party{feed_id=party}} -> ok;
        _ -> skip
    end.

index_post(#post{id=Id,type=post=T,message=M}=Post) ->
    case check_post(Post) of
        ok -> fulltext_search:add_index(T,Id,M);
        _ -> skip
    end;
index_post(#post{id=Id,type=thread=T,name=N,message=M}=Thread) ->
    case check_post(Thread) of
        ok ->
            fulltext_search:add_index(T,Id,N),
            fulltext_search:add_index(T,Id,M);
        _ -> skip
    end;
index_post({ok,#post{}=P}) -> index_post(P);
index_post(Id) when is_integer(Id) ->
    case kvs:get(post,Id) of
        {ok,E} -> index_post(E);
        _ -> skip
    end.
    
svg_search_button(normal) ->
    <<"<svg width='42px' height='44px' viewBox='0 0 42 44' version='1.1' xmlns='http://www.w3.org/2000/svg' xmlns:xlink='http://www.w3.org/1999/xlink'>
        <g stroke='none' stroke-width='1' fill='none' fill-rule='evenodd'>
            <path d='M20.5,46.75 L23.8,46.75 L23.8,31.65 C29.65,30.8 33.9,25.7 33.9,19.85 C33.9,13.4 28.6,8.1 22.15,8.1 C15.65,8.1 10.25,13.3 10.25,19.85 C10.25,25.8 14.6,30.85 20.5,31.65 L20.5,46.75 Z M22.15,28.45 C17.4,28.45 13.55,24.6 13.55,19.85 C13.55,15.15 17.45,11.4 22.15,11.4 C26.75,11.4 30.6,15.25 30.6,19.85 C30.6,24.55 26.85,28.45 22.15,28.45 Z' transform='translate(22.075000, 27.425000) rotate(-45.000000) translate(-22.075000, -27.425000) '></path>
        </g>
    </svg>">>;
svg_search_button(close) ->
    <<"<svg width='42px' height='44px' viewBox='0 0 42 44' version='1.1' xmlns='http://www.w3.org/2000/svg' xmlns:xlink='http://www.w3.org/1999/xlink'>
        <g stroke='none' stroke-width='1' fill='none' fill-rule='evenodd'>
            <path d='M35.1601562,33.7539062 L32.9140625,36 L21.234375,24.3203125 L9.5546875,36 L7.328125,33.7539062 L19.0078125,22.0742188 L7.328125,10.3945312 L9.5546875,8.16796875 L21.234375,19.8476562 L32.9140625,8.16796875 L35.1601562,10.3945312 L23.4804688,22.0742188 L35.1601562,33.7539062 Z'></path>
        </g>
    </svg>">>.