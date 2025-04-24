-module(erlach_subscription).
-author('Andy').

-compile(export_all).

-include("erlach.hrl").

key(Type,Id) -> {subscription,Type,Id}.
key(#post{type=Type,id=Id}) -> key(Type,Id).
list() -> lists:foldl(fun({{subscription,_,_},_}=V,A) -> [V|A]; (_,A) -> A end,[],erlang:get()).
remove(Tid) ->
    Rem=fun({subscription,post,I}=K) -> wf:info(?M,"REM: ~p",[K]), erlang:erase(key(post,I)), wf:wire(wf:f("lsrem(['sub','p',~b]);",[I]));
           ({subscription,thread,I}=K) -> wf:info(?M,"REM: ~p",[K]), erlang:erase(key(thread,I)), wf:wire(wf:f("lsrem(['sub','t',~b]);",[I]))
           end,
    lists:foreach(fun({{subscription,thread,I}=K,_}) when I =:= Tid ->
                   Rem(K),
                   wf:remove(panel_id(#post{type=thread,id=I},thread));
               ({{subscription,post,_}=K,{ok,post,{_,I,_,_}}}) when I =:= Tid -> Rem(K);
               (_) -> skip end,erlang:get()).
    


panel_key(#post{type=T,id=Id},Scope) -> {{?M,T,Scope},Id}.
panel_id(Target,Scope) -> spa:eternal_id(panel_key(Target,Scope)).

strip(#post{}=P) -> P#post{ name = <<>>, name_escaped = <<>>, message = <<>>, message_escaped = <<>> }.

ls_put(#post{type=thread,id=Tid}=T,{ok,thread,{Bid,{PT,PL,PC},{T1,L1,C1}}}=V) ->
    wf:wire(wf:f("ls(['sub','t',~b],[ts(),~b,~w,~w,~b,~w,~w,~b]);",[Tid,Bid,PT,PL,PC,T1,L1,C1])),
    erlang:put(key(T),V);
ls_put(#post{type=post,id=Pid}=P,{ok,post,{Bid,Tid,PL,L1}}=V) ->
    wf:wire(wf:f("ls(['sub','p',~b],[ts(),~b,~b,~w,~w]);",[Pid,Bid,Tid,PL,L1])),
    erlang:put(key(P),V).

put(#post{type=post,id=Pid,links=Links}=P,#st{board=#board{id=Bid}=B,thread=#post{id=Tid}=T}=S) ->
    wf:info(?M,"put/post ~p",[self()]),
    put(T,S),
    PrevLinks=case erlang:get(key(P)) of
        {ok,_,{_,_,PL,_}=Old} -> client(P,#hes{scope=Old,thread=T,board=B}), PL;
        _ -> Links
    end,
    ls_put(P,{ok,post,{Bid,Tid,PrevLinks,Links}});
put(#post{type=thread,id=Tid,links=Links}=T,#st{board=#board{id=Bid}=B,level=_From}=S) -> % TODO: + send copy to browser localStorage
    wf:info(?M,"put/thread ~p",[self()]),
    
    #feed{top=Top,count=Count}=F=case kvs:get(feed,{post,Tid}) of {ok,Fd} -> Fd; _ -> #feed{top=?UNDEF,count=0} end, 
    Prev=case erlang:get(key(T)) of
        {ok,_,{_,TLC,_}=Old} -> client(T,#hes{scope={Old,F},board=B}), TLC;
        _ -> {Top,Links,Count}
    end,
    
    ls_put(T,{ok,thread,{Bid,Prev,{Top,Links,Count}}}),
    
    %% Reseting posts count when selfposting
    case {spa:st(),self()} of {#st{thread=#post{id=Tid}},From} -> reset(#hes{thread=T,scope=thread_posts}); _ -> skip end.

update(#post{}=P,F,#st{}=S) ->
    wf:info(?M,"update: ~p",[self()]),
    case erlang:get(key(P)) of
        {ok,_,_} ->
            put(P,S), ok;
        _ -> skip
    end.

%%% Without text data (minimal)
reset(#hes{post=#post{id=Pid}=P,scope=Scope}) ->
    wf:info(?M,"reset/thread ~p",[self()]),
    case erlang:get(key(P)) of
        {ok,post,{Bid,Tid,PL,L}} ->
            Data={Bid,Tid,L,L},
            ls_put(P,{ok,post,Data}),
            wf:info(?M,"reset/post ~p Data: ~p",[self(),Data]),
            client(P,#hes{option=reset});
        _ -> skip
    end;
reset(#hes{post=?UNDEF,thread=#post{type=thread,id=Tid}=T,scope=Scope}) ->
    wf:info(?M,"reset/thread ~p",[self()]),
    #feed{top=Top,count=Count}=F=case kvs:get(feed,{post,Tid}) of {ok,Fd} -> Fd; _ -> #feed{top=?UNDEF,count=0} end, 
    case erlang:get(key(T)) of
        {ok,thread,{Bid,{PT,PL,PC},{T1,L1,C1}}} ->
            {ok,#board{}=B}=kvs:get(board,Bid),
            Data=case Scope of
                thread_answers -> {Bid,{PT,L1,PC},{T1,L1,C1}};
                thread_posts -> {Bid,{T1,PL,C1},{T1,L1,C1}}
            end,
            ls_put(T,{ok,thread,Data}),
            wf:info(?M,"reset/thread ~p Data: ~p",[self(),Data]),
            client(T,#hes{scope={Data,F},board=B,option=reset});
        _ -> skip
    end.

client(#post{type=post,id=Pid}=P,#hes{option=reset}) ->
    wf:info(?M,"client/post RESET ~p ~p",[self(),Pid]),
    Key=panel_key(P,post),
    Panel=panel_id(P,post),
    case spa:eternal_id_exist(Key) of
        true ->
            spa:eternal_id_erase(Key),
            wf:remove(Panel);
        false -> skip
    end;
client(#post{type=post,id=Pid,links=L2}=P,#hes{scope={Bid,Tid,PL,L1}=Old,thread=T,board=B}) ->
    wf:info(?M,"client/post ~p ~p",[self(),length(L2)-length(PL)]),
    case length(L2)-length(PL) of %length(L2 -- PL)
        Delta when Delta > 0 ->
            case spa:eternal_id_exist(panel_key(P,post)) of
                false ->
                    wf:insert_top(panel_id(T,container),render(P,#hes{scope={Old,Delta},thread=T,board=B}));
                true -> wf:update(panel_id(P,post),render(P,#hes{scope={Old,Delta},thread=T,board=B}))
            end;
        _ -> skip
    end;
client(#post{type=thread,id=Tid,links=L2}=T,#hes{scope={{Bid,{PT,PL,PC},{T1,L1,C1}}=Old,#feed{top=T2,count=C2}},board=B,option=O}) ->
    wf:info(?M,"client/thread ~p ~p",[self(),{length(L2)-length(PL), C2-PC}]),
    {DeltaL,DeltaC}={length(L2)-length(PL), C2-PC},
        case spa:eternal_id_exist(panel_key(T,thread)) of
            false -> wf:insert_top(sidebar,render(T,#hes{scope={Old,{DeltaL,DeltaC}},board=B}));
            true -> wf:update(panel_id(T,info),render(info,#hes{thread=T,scope={Old,{DeltaL,DeltaC}},board=B}))
        end.

render(#post{type=thread,id=Tid,links=L2}=T,#hes{scope=Scope,board=#board{id=Bid}=B}=Hes) ->
    {ok,#board{urn=BUrn}}=kvs:get(board,Bid),
    #panel{id=panel_id(T,thread),class= <<"element">>,body=[
        #span{class=[b,info],body=[<<"/">>,BUrn,<<"/">>,erlach_qs:id_to_urn(Tid),<<": ">>]},
        render(info,#hes{thread=T,scope=Scope,board=B}),
        #panel{id=panel_id(T,container),class= <<"container">>,body=[]}
    ]};
render(info,#hes{thread=#post{id=Tid,type=thread}=T,scope={{Bid,{PT,PL,PC},{T1,L1,C1}},{DeltaL,DeltaC}},board=B}) ->
    LP=#post{type=post,id=PT,urn=erlach_qs:id_to_urn(case PT of ?UNDEF ->Tid; _ -> PT end)},
    #span{id=panel_id(T,info),body=[
        #button{class=sea,body=[<<"Replies +"/utf8>>,wf:to_list(DeltaL)], % new answers of thread
           title= <<"New Replies">>, postback=#pubsub{render=?M,target=subscription,action=view,element=erlach_qs:mp({post,B,T,T}),
               data=#hes{thread=strip(T),scope=thread_answers},from=self()}},
        #button{class=sea,body=[<<"Posts +"/utf8>>,wf:to_list(DeltaC)], % new posts in thread
            title= <<"New Posts">>,
            postback=#pubsub{render=?M,target=subscription,action=view,element=erlach_qs:mp({post,B,T,LP}),
                data=#hes{thread=strip(T),scope=thread_posts},from=self()}},
        #button{class=[sea,checked],body=[<<"&#10006;">>],
            postback=#pubsub{render=?M,target=subscription,action=remove,data=Tid,from=self()}}
    ]};
render(#post{type=post,urn=Urn,links=L2}=P,#hes{scope={Old,Delta},thread=T,board=B}=Hes) ->
    #button{id=panel_id(P,post),class=[orange,slim],
        body=[<<"#">>,Urn,<<"+">>,wf:to_list(Delta)], % answers of post        
            postback=#pubsub{render=?M,target=subscription,action=view,element=erlach_qs:mp({post,B,T,P}),
                data=#hes{post=strip(P),thread=strip(T)},from=self()}}.

event(#pubsub{target=subscription,action=put,element=#post{}=P,data=#st{}=S,from=F}) ->
    wf:info(?M,"Pubsub subscription PUT from ~p",[self()]),
    update(P,F,S);
event(#pubsub{target=subscription,action=view,element=#postback{}=P,data=#hes{}=H,from=F}) ->
    wf:info(?M,"Pubsub subscription VIEW from ~p to ~p",[F,self()]),
    reset(H),
    spa:redirect(P);
event(#pubsub{target=subscription,action=remove,data=Tid,from=F}) ->
    wf:info(?M,"Pubsub subscription REMOVE from ~p to ~p",[F,self()]),
    remove(Tid);
event(Unknown) -> ?EVENT_ROUTER:unknown(?M,Unknown).


pickup_data() -> wf:wire(spa_lambda_event:new(?M,#pubsub{render=?M,target=subscription,action=pickup},"sub()")).

api_event(#pubsub{target=subscription,action=pickup},[SubList],_State) ->
    wf:info(?M,"Api Event: subscription",[]),
    LsRem=fun(Type,Id) ->
        wf:wire(wf:f("lsrem(['sub','~s',~b]);",[Type,Id]))
    end,
    TidList1=lists:foldl(fun({{<<"t">>,Tid},{Timestamp,Bid,PT,[PL],PC,T1,[L1],C1}},Acc) ->
            wf:info(?M,"SUB P ~p ~p ~p (~p ~p) (~p ~p)",[Tid,Timestamp,Bid,PT,T1,PC,C1]),

            case {kvs:get(board,Bid),kvs:get(post,Tid)} of
                {{ok,B},{ok,#post{type=thread}=T}} ->
                    erlang:put(key(thread,Tid),{ok,thread,{Bid,{PT,PL,PC},{T1,L1,C1}}}), % TODO: REFACTOR THIS HELL
                    put(T#post{links=L1},#st{board=B}),
                    [Tid|Acc];
                _ ->
                    LsRem("t",Tid),
                    Acc
            end;
        (_,Acc) -> Acc end,[],SubList),
    TidList2=lists:foldl(fun({{<<"p">>,Pid},{Timestamp,Bid,Tid,[PL],[L1]}},Acc) ->
            wf:info(?M,"SUB P ~p ~p ~p ~p",[Pid,Timestamp,Bid,Tid]),

            case {kvs:get(board,Bid),kvs:get(post,Tid),kvs:get(post,Pid)} of
                {{ok,B},{ok,#post{type=thread}=T},{ok,#post{type=post}=P}} ->
                    erlang:put(key(post,Pid),{ok,post,{Bid,Tid,PL,L1}}), % TODO: REFACTOR THIS HELL
                    put(P#post{links=L1},#st{board=B,thread=T}),
                    [Tid|Acc];
                _ ->
                    LsRem("p",Pid),
                    Acc
            end;
        (_,Acc) -> Acc end,TidList1,SubList),
    [ wf:reg({subscription,thread,TX}) || TX <- lists:usort(TidList2)];
    % wf:info(?M,"SUB ~p ~p",[self(),list()]);
api_event(Unknown,_,_) -> ?EVENT_ROUTER:unknown(?M,Unknown).
