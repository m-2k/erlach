-module(erlach_utils).
-author('Andy').
-compile(export_all).

-include("erlach.hrl").

make_bw_thread(Urn) ->
    {ok,T}=kvs:get(post,erlach_qs:urn_to_id(Urn)),
    erlach_feeds:update(T,fun(#post{name=N,name_escaped=NE}=T2) ->
        N2= <<N/binary," [BW]">>,
        {ok,T2#post{name=N2,name_escaped=wf:to_binary(wf:html_encode(wf:to_list(N2)))}} end).

% Render State
rst() -> erlang:get(rst).
rst(Value) -> erlang:put(rst,Value).
rst_erase() -> rst(#rst{}).

hidden(H) -> R=rst(), rst(R#rst{hidden_elements=R#rst.hidden_elements++[H]}).
hidden() -> R=rst(), H=R#rst.hidden_elements,rst(R#rst{hidden_elements=[]}), H.

enc(T) -> wf:html_encode(wf:to_list(T)).

post_id(P) -> post_id(P,spa:st()).
post_id(Pid,#st{action=search}) when is_integer(Pid) -> "es-"++spa_utils:hash({post_search,Pid});
post_id(Pid,_) when is_integer(Pid) -> "e-"++spa_utils:hash({post,Pid});
post_id(#post{id=Pid},S) -> post_id(Pid,S);
post_id(#attachment{thread=Tid,post=?UNDEF},S) -> post_id(Tid,S);
post_id(#attachment{post=Pid},S) -> post_id(Pid,S).

image_panel_id(FileName) -> image_panel_id(FileName,spa:st()).
image_panel_id(FileName,#st{action=search}) -> "ims-"++spa_utils:hash({image_key_search,FileName});
image_panel_id(FileName,_) -> "im-"++spa_utils:hash({image_key,FileName}).

% link to board
board_link(H,#board{name=Bn,urn=Urn}=B,#st{}) ->
    H#a{class=[link],
        body=enc([?RPL,"/",Urn,"/"]),
        title=enc([Bn,<<" – "/utf8>>,Urn]),
        href=erlach_qs:ml({board,B}),
        postback=erlach_qs:mp({board,B})}.

% link to self thread
link(H,#post{type=thread,id=Tid,urn=Urn}=T,#st{board=#board{name=Bn}=B,thread=#post{id=Tid,name_escaped=Tn}}) ->
    H#a{class=[link,thread],
        body=enc([?RPL,Urn]),
        title=enc([Bn,<<" – "/utf8>>,Tn]),
        href=erlach_qs:ml({thread,B,T}),
        postback=erlach_qs:up_state({thread,B,T}),
        data_fields=[{<<"data-link">>,post_id(T)}] };
% link to post in self thread
link(H,#post{type=post,feed_id={post,Tid},urn=Urn}=P,#st{board=#board{name=Bn}=B,thread=#post{id=Tid,name_escaped=Tn}=T}) ->
    H#a{class=[link],
        body=enc([?RPL,Urn]),
        title=enc([Bn,<<" – "/utf8>>,Tn,<<" – "/utf8>>,Urn]),
        href=erlach_qs:ml({post,B,T,P}),
        postback=erlach_qs:up_state({post,B,T,P}),
        data_fields=[{<<"data-link">>,post_id(P)}] };
% link to external thread
link(H,#post{type=thread,feed_id={thread,Bid},urn=Urn,name_escaped=Tn}=T,#st{}) ->
    {ok,#board{name=Bn}=B}=kvs:get(board,Bid),
    Href=erlach_qs:ml({thread,B,T}),
    H#a{class=[link,external],
        title=enc([Bn,<<" – "/utf8>>,Tn,<<" (на другой странице)"/utf8>>]),
        body=[enc([?RPL,Urn]),#sup{body="ext"}],
        href=Href,
        postback=(erlach_qs:mp({thread,B,T}))#postback{route_option=highlight} };
% link to external post
link(H,#post{type=post,id=Pid,feed_id={post,Tid},urn=Urn}=P,#st{}) ->
    {ok,#post{type=thread,feed_id={thread,Bid},name_escaped=Tn}=T}=kvs:get(post,Tid),
    {ok,#board{name=Bn}=B}=kvs:get(board,Bid),
    Href=erlach_qs:ml({post,B,T,P}),
    H#a{class=[link],
        title=enc([Bn,<<" – "/utf8>>,Tn,<<" – "/utf8>>,Urn,<<" (на другой странице)"/utf8>>]),
        body=[enc([?RPL,Urn]),#sup{body="ext"}],
        href=Href,
        postback=erlach_qs:mp({post,B,T,P}) }.

partial(#view{partial=true,table=Table,feed=Feed,start=Start,count=Count,direction=Direction}=V) ->
    case kvs:get(feed,Feed) of
        {ok,C} ->
            {Direction2,Head}=case Direction of
                next -> {#iterator.next,#container.rear};
                _ -> {#iterator.prev,#container.top}
            end,
            Start2=case is_integer(Start) of false -> element(Head,C); true -> Start end,
            Count2=case is_integer(Count) of false -> element(#container.count,C); true -> Count end,
            
            List=kvs:fold(fun(E,A) -> [E|A] end,[],Table,Start2,Count2,Direction2),
            case List of
                [] -> {[],stop};
                _ ->
                    case element(Direction2,hd(List)) of
                        Id when is_integer(Id) -> {List,V#view{start=Id}};
                        _ -> {List,stop}
                    end
            end;
        _ -> {[],stop}
    end.

%%% when u want stopped fold, then just raise any error https://github.com/synrc/kvs/blob/3.4/src/kvs.erl#L216
fold(Fun,Table,Container,Feed,Acc) ->
    case kvs:get(Container,Feed) of
        {ok,C} -> kvs:fold(Fun,Acc,Table,element(#container.top,C),element(#container.count,C),#iterator.prev,#kvs{mod=?DBA});
        _ -> Acc
    end.

utf8_to_list(Any) -> unicode:characters_to_list(list_to_binary([Any]),utf8).
char_length(Any) -> length(utf8_to_list(Any)).

cut(Any,Limit) -> cut(Any,Limit,<<"…"/utf8>>).
cut(Any,Limit,End) ->
    Chars=utf8_to_list(Any),
    list_to_binary(case length(Chars) > Limit of
        true -> [unicode:characters_to_binary(lists:sublist(Chars,Limit),utf8),End];
        false -> [Any]
    end).
    
hash(Data) -> crypto:hash(sha512,Data).
