-module(erlach_utils).
-author('Andy').
-compile(export_all).
-include("erlach.hrl").

% Render State
rst() -> erlang:get(rst).
rst(Value) -> erlang:put(rst,Value).
rst_erase() -> rst(#rst{}).

hidden(H) -> R=rst(), rst(R#rst{hidden_elements=R#rst.hidden_elements++[H]}).
hidden() -> R=rst(), H=R#rst.hidden_elements,rst(R#rst{hidden_elements=[]}), H.

enc(T) -> wf:html_encode(wf:to_list(T)).

% link to board
board_link(H,#board{name=Bn,urn=Urn}=B,#st{}) ->
    H#a{body=enc([?RPL,"/",Urn,"/"]),title=enc([Bn,<<" – "/utf8>>,Urn]),
        href=erlach_qs:ml({board,B}),
        postback=erlach_qs:mp({board,B})}.

% link to self thread
link(H,#post{type=thread,id=Tid}=T,#st{board=#board{name=Bn}=B,thread=#post{id=Tid,name_escaped=Tn}}) ->
    H#a{body=enc([?RPL,<<"thread">>]),title=enc([Bn,<<" – "/utf8>>,Tn]),
        href=erlach_qs:ml({thread,B,T}),
        postback=erlach_qs:up_state({thread,B,T}),
        onclick=[<<"scrollToPost(\\'">>,spa:id(T),<<"\\');">>]};
% link to post in self thread
link(H,#post{type=post,feed_id={post,Tid},urn=Urn}=P,#st{board=#board{name=Bn}=B,thread=#post{id=Tid,name_escaped=Tn}=T}) ->
    H#a{body=enc([?RPL,Urn]),title=enc([Bn,<<" – "/utf8>>,Tn,<<" – "/utf8>>,Urn]),
        href=erlach_qs:ml({post,B,T,P}),
        postback=erlach_qs:up_state({post,B,T,P}),
        onclick=[<<"scrollToPost(\\'">>,spa:id(P),<<"\\');">>]};
% link to external thread
link(H,#post{type=thread,feed_id={thread,Bid},name_escaped=Tn}=T,#st{}) ->
    {ok,#board{name=Bn}=B}=kvs:get(board,Bid),
    Href=erlach_qs:ml({thread,B,T}),
    H#a{body=enc([?RPL,Href]),title=enc([Bn,<<" – "/utf8>>,Tn]),
        href=Href,postback=(erlach_qs:mp({thread,B,T}))#postback{route_option=highlight} };
% link to external post
link(H,#post{type=post,id=Pid,feed_id={post,Tid},urn=Urn}=P,#st{}) ->
    {ok,#post{type=thread,feed_id={thread,Bid},name_escaped=Tn}=T}=kvs:get(post,Tid),
    {ok,#board{name=Bn}=B}=kvs:get(board,Bid),
    Href=erlach_qs:ml({post,B,T,P}),
    H#a{body=enc([?RPL,Href]),title=enc([Bn,<<" – "/utf8>>,Tn,<<" – "/utf8>>,Urn]),
        href=Href,postback=erlach_qs:mp({post,B,T,P}) }.
            

partial(#view{queue=Q,partial=true}=V) ->
    C=wf:config(erlach,partial_count,50),
    T=wf:config(erlach,partial_timeout,10),
    {Instant,Remaining}=lists:split(case length(Q) of L when L > C -> C; L -> L end,Q),
    erlang:send_after(T,self(),{server,V#view{queue=Remaining}}),
    Instant.

%%% when u want stopped fold, then just raise any error https://github.com/synrc/kvs/blob/3.4/src/kvs.erl#L216
fold(Fun,Table,Container,Feed,Acc) ->
    case kvs:get(Container,Feed) of
        {ok,C} -> kvs:fold(Fun,Acc,Table,element(#container.top,C),element(#container.count,C),#iterator.prev,#kvs{mod=?DBA});
        _ -> Acc
    end.
    
cut(Bin,Limit) ->
    Chars=unicode:characters_to_list(Bin,utf8),
    case length(Chars) > Limit of
        true -> <<(unicode:characters_to_binary(lists:sublist(Chars,Limit),utf8))/binary,"…"/utf8>>;
        false -> Bin
    end.