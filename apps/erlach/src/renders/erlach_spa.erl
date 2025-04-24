-module(erlach_spa).
-compile(export_all).
% -compile({parse_transform, shen}).
-author('Andy').

-include("erlach.hrl").

main() -> wf:info(?M,"main",[]).

event(init) ->
    wf:info(?M,"init ~p",[self()]),
    Route=?CTX#cx.path,
    
    normalize_page(),
    navigate(Route),
    erlach_qs:history_init(),
    erlach_subscription:pickup_data(),
    
    wf:wire(wf:f("time_warp=Date.now()-~b;",[spa_utils:now_js(erlang:timestamp())])),
    
    case spa:st() of
        #st{services=comments} -> skip;
        _ ->
            spa:update(?SPA,manage),
            spa:update(?SPA,footer),
            spa:update(?SPA,'erlach-logo')
    end,
    
    wf:info(?M,"init ~p",[ok]);
event({server,{debug,st}}) -> wf:info(?M,"DEBUG STATE~n~p",[spa:st()]);
event({server,{debug,mq}}) -> wf:info(?M,"DEBUG MQ~n~p",[[ P || {{pool,_},_}=P <- get()]]);
event({client,{history,Secret}}) -> navigate((n2o_secret:depickle(Secret))#query{history=true});
event(terminate) ->
    terminate_render();
event(E) -> wf:info(?M,"Proxy event",[]), ?EVENT_ROUTER:proxy(E).

update_all(Render) ->
    case spa:st() of
        #st{services=comments} -> skip;
        _ -> spa:update(?SPA,breadcrumbs)
    end,
    spa:update(Render,content).

normalize_page() ->
    ok.
init_render(Route) ->
    spa:id_erase(),
    erlach_utils:rst_erase(),
    PageId=wf:to_binary(erlang:unique_integer()), % for FTP
    wf:reg(PageId),    
    wf:wire("init('"++wf:to_list(PageId)++"');"),
    Route#route{page_id=PageId}.
    
terminate_render() ->
    wf:wire("terminate();"),
    case spa:st() of
        #st{route=#route{render=R,page_id=PageId}}=S ->
            erlach_stat:time_stop(S),
            wf:unreg(PageId),
            R:terminate();
        _ -> skip
    end.
navigate(#query{}=Q) -> navigate(erlach_routes:route(Q));
navigate(#postback{action=view,history=H,query=#query{q2=Urn2,q3=Urn3}=Q,route_option=O}=P) ->
    wf:info(?M,"Navigate ~p",[P]),
    case erlach_qs:is_in_thread_postback(P) of
        true ->
            Fun=case O of state_update_only -> up_state; _ -> scroll_to end,
            erlach_thread:Fun(erlach_qs:urn_to_id(wf:coalesce([Urn3,Urn2])));
            % skip;
        _ ->
            navigate((erlach_routes:route(Q))#route{option=O}),
            case H of true -> erlach_qs:history_push(); _ -> skip end
    end;
navigate(#route{render=Render}=R) ->
    wf:info(?M,"New Route: ~p",[R]),

    terminate_render(),
    R2=init_render(R),
    
    case Render:init(R2) of
        {redirect,Postback} ->
            wf:info(?M, "navigate redirect to: ~p",[Postback]),
            spa:redirect(Postback);
        {ok,#st{route=R3}=S} ->
            erlach_stat:time_start(S),
            spa:st(S#st{access=erlach_auth:access()}),
            update_all(Render),
            Finalize=case Render:finalize(spa:st()) of F when is_list(F) -> F; _ -> [] end,
            case proplists:get_value(scroll,Finalize) of
                Pid when is_integer(Pid) -> wf:wire(["scrollWait('",erlach_utils:post_id(Pid),"');"]);
                _ -> wf:wire(["scrollWait();"])
            end,
            case proplists:get_bool(wait_finalize,Finalize) of
                true -> skip;
                false -> wf:wire("finalize();")
            end,
            wf:info(?M, "~p navigate to: ~p",[self(),R3]),
            []
        % Error ->
        %     wf:error(?M, "navigate error: ~p",[Error]),
        %     erlach_utils:redirect(erlach_qs:mp(main))
    end.


render(breadcrumbs=Panel,#st{board=#board{urn=Urn,name=Name}=B,thread=#post{type=thread}}) ->
    #panel{id=Panel,body=#a{body=[$/,Urn,<<"/ – "/utf8>>,Name],href=erlach_qs:ml({board,B}),postback=erlach_qs:mp({board,B})}};
render(breadcrumbs=Panel,#st{}=S) -> #panel{id=Panel};
render('erlach-logo'=Panel,#st{}) ->
    #a{id=Panel,body=wf:jse(erlach_svg:logo()),href=erlach_qs:ml(main),postback=erlach_qs:mp(main)};
render(manage=Panel,#st{}=S) ->
    Btn=spa:temp_id(),
    #panel{id=Panel,body=[
        #a{id=Btn,class=selector,body= <<"Ответы"/utf8>>,
            postback=#view{render=erlach_main,target=sidebar,option=true,element=Btn}}
    ]};
render(footer=Panel,#st{access=A}) ->
    ExtClass= <<"b ext">>,
    IntClass= <<"b">>,
    #panel{id=Panel,body=[
        #panel{class= <<"related-links">>,body=[
            #link{class=ExtClass, target="_blank", body= <<"Блог"/utf8>>, href= <<"http://blog.erlach.co">>},
            #link{class=ExtClass, target="_blank", body= <<"LING"/utf8>>, href= <<"http://erlangonxen.org">>},
            #link{class=ExtClass, target="_blank", body= <<"BPG"/utf8>>, href= <<"http://bellard.org/bpg/">>},
            #a{class=IntClass, body= <<"Настройки"/utf8>>, source=[x],
                postback=#view{render=erlach_settings,target=window,option=show}},
            #a{class=IntClass, body= <<"Инфо"/utf8>>, href=erlach_qs:ml(about), postback=erlach_qs:mp(about)},
            case A of ?UNDEF -> []; _ -> #a{class=IntClass, body= <<"Выход"/utf8>>, postback=#auth{logout=true}} end
            ]}
    ]}.