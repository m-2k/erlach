-module(erlach_spa).
-compile(export_all).
-author('Andy').

-include("erlach.hrl").

main() -> wf:info(?M,"main",[]).

redirect(#postback{}=P) -> self() ! {server,P}.

event(init) ->
    wf:info(?M,"init ~p",[self()]),
    wf:update(header,?SPA:render(header,#st{})),
    wf:update(footer,?SPA:render(footer,#st{})),
    wf:update('popup-messages',#panel{id='popup-messages',class= <<"line-up">>}),
    Route=?CTX#cx.path,
    navigate(Route),
    erlach_qs:history_init(),
    erlach_subscription:pickup_data(),
    wf:info(?M,"init ~p",[ok]);
event({client,{history,Secret}}) -> navigate((n2o_secret:depickle(Secret))#query{history=true});
event(terminate) ->
    terminate_render(),
    erlach_image:clear_temporary(erlach_utils:rst());
event(E) -> ?EVENT_ROUTER:proxy(E).

init_render(Route) ->
    spa:id_erase(),
    erlach_utils:rst_erase(),
    PageId=wf:to_binary(erlang:unique_integer()), % for FTP
    wf:reg(PageId),    
    wf:wire("init('"++wf:to_list(PageId)++"');"),
    Route#route{page_id=PageId}.
    
terminate_render() ->
    case spa:st() of
        #st{route=#route{render=R,page_id=PageId}}=S ->
            erlach_stat:time_stop(S),
            wf:unreg(PageId), 
            R:terminate();
        _ -> skip
    end.
navigate(#query{}=Q) -> navigate(erlach_routes:route(Q));
navigate(#postback{action=view,history=H,query=#query{q3=Urn}=Q,route_option=O}=P) ->
    wf:info(?M,"Navigate ~p",[P]),
    case erlach_qs:is_in_thread_postback(P) of
        true when is_binary(Urn) andalso size(Urn) > 0 ->
            erlach_thread:scroll_to(erlach_qs:urn_to_id(Urn));
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
            redirect(Postback);
        {ok,#st{route=R3}=S} ->
            erlach_stat:time_start(S),
            S2=S#st{access=erlach_auth:access()},
            wf:update(content,Render:render(content,S2)),
            wf:update(breadcrumbs,?SPA:render(breadcrumbs,S2)),
            wf:update(footer,?SPA:render(footer,S2)),
            Render:finalize(S2),
            wf:info(?M, "navigate to: ~p",[R3]),
            spa:st(S2),
            []
        % Error ->
        %     wf:error(?M, "navigate error: ~p",[Error]),
        %     erlach_utils:redirect(erlach_qs:mp(main))
    end.

render(breadcrumbs=Panel,#st{board=#board{urn=Urn,name=Name}=B,thread=#post{type=thread}}) ->
    #panel{id=Panel,body=#hookup{body=[$/,Urn,<<"/ – "/utf8>>,Name],href=erlach_qs:ml({board,B}),postback=erlach_qs:mp({board,B})}};
render(breadcrumbs=Panel,#st{}) -> #panel{id=Panel};
render('erlach-logo'=Panel,#st{}) -> #hookup{id=Panel,body=wf:jse(logo()),href=erlach_qs:ml(main),postback=erlach_qs:mp(main)};
render(manage=Panel,#st{}=S) ->
    Btn=spa:temp_id(),
    #panel{id=Panel,body=[
        #hookup{id=Btn,class=selector,body= <<"Notify">>,postback=#view{mod=erlach_main,target=sidebar,option=true,element=Btn}}
    ]};
    
render(header=Panel,#st{}=S) ->
    #header{id=Panel,class= <<"fl cent line-down">>,body=#panel{class= <<"fl page">>,body=[
        render(breadcrumbs,S),
        render('erlach-logo',S),
        render(manage,S)
        ]}};
render(footer=Panel,#st{access=A}) ->
    ExtClass= <<"b checked">>,
    IntClass= <<"b checked">>,
    #panel{id=Panel,body=[
        #panel{class= <<"related-links">>,body=[
            #link{class=ExtClass, target="_blank", body= <<"LING"/utf8>>, href= <<"http://erlangonxen.org">>},
            #link{class=ExtClass, target="_blank", body= <<"Tails"/utf8>>, href= <<"https://tails.boum.org">>},
            #link{class=ExtClass, target="_blank", body= <<"Tor"/utf8>>, href= <<"https://www.torproject.org">>},
            #link{class=ExtClass, target="_blank", body= <<"NoSQL"/utf8>>, href= <<"http://www.erlang.org/doc/apps/mnesia/">>},
            #link{class=ExtClass, target="_blank", body= <<"BPG"/utf8>>, href= <<"http://bellard.org/bpg/">>},
            #hookup{class=IntClass, body= <<"About Erlach"/utf8>>, href=erlach_qs:ml(about), postback=erlach_qs:mp(about)},
            case A of ?UNDEF -> []; _ -> #hookup{class=IntClass, body= <<"Logout"/utf8>>, postback=#auth{logout=true}} end
            ]}
    ]}.

logo() -> % Different as original
    <<"<svg width='100px' height='20px' viewBox='-2 0 284 50' version='1.1'
        xmlns='http://www.w3.org/2000/svg' xmlns:xlink='http://www.w3.org/1999/xlink'>
        <g stroke='#26c6da' stroke-width='2' fill='black' fill-rule='evenodd'>
            <path d='M21.6,2 L22.2,2 L22.9,1.8 L23,2 L23,24.6 L22.7,24.9 L0.2,24.9 C0,24.9 0,24.8 
            0,24.5 C0,19.4 1.8,14.3 5.6,9.2 C9.9,4.4 15.3,2 21.6,2 L21.6,2 Z M24.3,1.8 L25.5,1.8 
            C29.9,1.8 34.6,3.6 39.7,7.2 C42.6,9.9 44.3,12.2 45.1,14.2 C46.5,17.9 47.2,21.2 
            47.2,24.1 L47.1,24.1 L47.2,24.2 L47.2,24.6 L46.9,24.9 L24.3,24.9 L24,24.6 L24,2.1 
            L24.3,1.8 Z M22.7,25.7 L23,26 L23,48.7 C23,48.9 22.9,49 22.6,49 C18.7,48.5 15.2,47.6 
            12.3,46.4 C4.1,42.3 0,35.5 0,25.9 L0.1,25.7 L22.7,25.7 Z M70.8,1.8 L70.9,2 L70.9,24.6 
            L70.6,24.9 L48.1,24.9 C48,24.9 47.9,24.8 47.9,24.5 C47.9,20.4 49.2,16.1 51.8,11.4 
            C56,5 62.3,1.8 70.8,1.8 L70.8,1.8 Z M72.2,1.7 L94.8,1.7 L95.1,2 L95.1,3.1 C95.1,6.2 
            93.9,10.1 91.5,14.9 C87.2,21.6 80.8,24.9 72.1,24.9 L71.9,24.8 L71.9,2 L72.2,1.7 Z 
            M70.6,25.7 L70.9,26 L70.9,48.7 L70.6,49 L70.4,49 C66.2,48.4 62.7,47.5 59.8,46.1 
            C51.9,42.1 47.9,35.3 47.9,25.9 L48,25.7 L70.6,25.7 Z M96.1,1.8 L96.2,1.8 C101.3,1.8 
            106.4,3.7 111.5,7.5 C116.3,11.8 118.7,17.2 118.7,23.5 L118.7,24.6 L118.4,24.9 
            L96.1,24.9 L95.8,24.6 L95.8,2.1 L96.1,1.8 Z M95.9,25.7 L118.6,25.7 L118.8,26 
            L118.8,48.7 L118.6,49 L118.1,49 C113.9,48.4 110.3,47.4 107.3,46 C102.3,43.4 98.9,39.6 
            97.2,34.7 C96.2,31.7 95.6,28.9 95.6,26.5 L95.8,26.5 L95.6,26.3 L95.6,26 L95.9,25.7 Z 
            M120.3,25.7 C125.2,26.4 129.2,27.5 132.2,29.1 C139.3,33.4 142.9,39.9 142.9,48.8 
            L142.8,49 L120.1,49 L119.8,48.7 L119.8,26 C119.8,25.8 120,25.7 120.3,25.7 L120.3,25.7 
            Z M166.5,1.8 L166.6,2 L166.6,24.6 L166.3,24.9 L143.7,24.9 C143.5,24.9 143.4,24.8 
            143.4,24.5 C143.4,19.9 145.1,15 148.3,10 C152.6,4.6 158.7,1.8 166.5,1.8 L166.5,1.8 Z 
            M167.8,1.8 L169.2,1.8 C173.4,1.8 178,3.6 183.1,7 C188.1,11.4 190.7,17.3 190.7,24.8 
            L190.5,24.9 L167.8,24.9 L167.5,24.6 L167.5,2.1 L167.8,1.8 Z M166.2,25.7 L166.5,26 
            L166.5,48.7 L166.2,49 L166.1,49 C162.4,48.5 159.1,47.7 156.2,46.6 C147.7,42.5 
            143.4,35.6 143.4,25.9 L143.6,25.7 L166.2,25.7 Z M190.4,25.7 L190.7,26 L190.7,48.7 
            L190.4,49 L190,49 C186.1,48.4 183.1,47.6 180.7,46.7 C171.9,42.7 167.5,35.8 167.5,25.9 
            L167.6,25.7 L190.4,25.7 Z M215.7,0.6 L215.8,0.6 C221.4,0.6 226.6,3 231.4,7.7 
            L231.4,7.9 L215.2,24.1 L199.1,7.9 L199.1,7.7 C204.3,3 209.9,0.6 215.7,0.6 L215.7,0.6 
            Z M198.4,8.5 L214.5,24.6 L214.5,24.8 L198.4,40.9 L198.2,40.9 C193.5,35.4 191.2,30.1 
            191.2,25 L191.2,23.2 C191.2,18.2 193.6,13.3 198.4,8.5 L198.4,8.5 Z M215.2,25.6 
            L215.4,25.6 L231.6,41.8 L231.6,41.9 C226,46.6 220.4,49 214.8,49 L214.7,49 C209,49 
            203.8,46.6 199.1,41.8 L215.2,25.6 Z M232.4,2 L233.1,2 C237.8,2 242.6,3.8 247.7,7.3 
            C252.7,11.6 255.2,17.4 255.2,24.9 L232.1,24.9 L232.1,2.3 L232.4,2 Z M279.1,1.8 
            L279.2,2 L279.2,24.6 L279,24.9 L256.5,24.9 L256.2,24.6 L256.2,23.8 C256.2,18.6 
            258.3,13.4 262.5,8.2 C266.7,3.9 272.3,1.8 279.1,1.8 L279.1,1.8 Z M279,25.7 L279.2,26 
            L279.2,48.7 C279.2,48.9 279.1,49 278.8,49 C274.4,48.4 271,47.6 268.5,46.4 
            C260.3,42.4 256.2,35.5 256.2,25.9 L256.3,25.7 L279,25.7 Z M232.4,25.9 L254.9,25.9 
            L255.2,26.2 C255.2,29.1 254.2,33 252.1,37.8 C247.8,45.2 241.2,49 232.3,49 L232.1,48.8 
            L232.1,26.2 L232.4,25.9 Z' id='erlach' fill='#505050'></path></g>
        </svg>">>.