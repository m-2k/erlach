-module(erlach_board).
-author('andy').
-compile(export_all).

-include_lib("n2o/include/wf.hrl").
-include_lib("nitro/include/nitro.hrl").

-include_lib("erlach_db/include/erlach_db.hrl").
-include("erlach.hrl").

title(#st{}) -> <<"Board – Erlach"/utf8>>.
urn() -> ?UNDEF.

init(#route{board=Burn}=Route) ->
    wf:info(?M,"init",[]),
    case kvs:index(board,urn,Burn) of
        [#board{id=Bid}=B] ->
            wf:reg({board,Bid}),
            {ok,#st{route=Route,action=view,board=B}};
        _ -> {redirect,erlach_qs:mp(main)}
    end.
terminate() ->
    wf:info(?M,"TERMINATE ~p",[self()]),
    #st{board=#board{id=Bid}}=erlang:get(state),
    wf:unreg({board,Bid}),
    wf:info(?M,"TERMINATE ~p OK",[self()]).

render(content=Panel,#st{board=#board{id=Bid,name=Name,desc=Desc}=B}=S) ->
    wf:info(?M,"Board: ~p",[Bid]),
    #panel{id=Panel,body=[
        #panel{class= <<"content-title">>,body=Name},
        #span{class= <<"remark">>,body=Desc},
        #panel{id= <<"threads">>, body=[
            render(controls,S),
            lists:foldl(fun(T,Acc) -> [render(T,#hes{board=B},S) | Acc] end,[],kvs:entries(kvs:get(feed,{thread,Bid}),thread,undefined))
            ]}
    ]};
render(controls=Panel,#st{}) ->
    #panel{id=Panel,class= <<"center">>,body=#button{body= <<"New thread">>,postback=#create{target=thread}}};
render({'thread-topic',#thread{urn=Urn,name_escaped=Name}=T,#board{}=B},#st{}) ->
    #hookup{class= <<"link">>,href=erlach_qs:ml({thread,B,T}),postback=erlach_qs:mp({thread,B,T}),
        body=#panel{class= <<"post-topic">>,body=case Name of <<>> -> [<<"thread#">>,Urn]; _ -> Name end}}.

render(#thread{id=Tid,image=Image,message_escaped=Message}=T,#hes{board=B},#st{}=S) ->
    Panel=erlach_utils:id(T),
    ImagePanel=erlach_utils:id({thread,image},Tid),
    #panel{id=Panel,class= <<"post">>,
        body=#panel{class= <<"post-content">>,body=[
            erlach_thread:render({image_panel,ImagePanel,Image},S),
            render({'thread-topic',T,B},S),
            #panel{class= <<"post-message">>,body=Message}
            ]}}.
    
event(#pubsub{action=add,data=#thread{}=T,from=Proc}) ->
    wf:info(?M,"RECEIVE add #thread{} ~p",[self()]),
    case self() of
        Proc ->
            #st{board=#board{}=B}=S=erlang:get(state),
            Panel=erlach_utils:id(T),
            Target="qs('#"++Panel++" .post-topic')",
            
            wf:wire(#jq{target=Target,property=outerHTML,args=simple,right=render({'thread-topic',T,B},S),format="'~s'"});
        _ ->
            #st{board=#board{id=Bid}=B}=S=erlang:get(state),
            wf:insert_top(threads,render(T,#hes{board=B},S))
    end;

event(#create{target=thread}) -> wf:update(controls,#media_input{id=input,image= <<"input-image">>,type=thread});
event(#cancel{target=input,panel=Panel}) -> wf:update(Panel,render(controls,erlang:get(state)));
event(#add{target=post}=E) ->
    erlach_thread:event(E),
    wf:insert_top(threads,render(controls,erlang:get(state)));
event(#ftp{}=Ftp) ->
    wf:info(?M,"FTP: ~p",[Ftp]),
    erlach_thread:event(Ftp);
event(#pubsub{action=bpg,data=BpgPath,from=From,target=Target}=E) ->
    wf:info(?M,"RECEIVE converted image ~p ~p",[self(),E]),
    case Target of
        % self
        {container,ImageContainer} when self() =:= From ->
            #bpg_info{picture_width=BpgW,picture_height=BpgH}=erlach_thread:send_bpg(ImageContainer,BpgPath),
            wf:wire(wf:q("setCanvasSize('~s',~b,~b)",[ImageContainer,BpgW,BpgH]));
        % page reloaded and post not stored
        {container,ImageContainer} -> skip;
        % all (stored)
        {element,Key,Id} ->
            ImageContainer=erlach_utils:id(Key,Id),
            #bpg_info{picture_width=BpgW,picture_height=BpgH}=erlach_thread:send_bpg(ImageContainer,BpgPath),
            wf:wire(wf:q("setCanvasSize('~s',~b,~b)",[ImageContainer,BpgW,BpgH]))
    end;
event({server,{upload_state,ImageContainer,Text}}) ->
    wf:wire("uploadFileShowState('"++wf:to_list(ImageContainer)++"','"++wf:to_list(Text)++"');");
event(Unknown) -> ?EVENT_ROUTER:unknown(?M,Unknown).
