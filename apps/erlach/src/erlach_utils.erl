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

% link to self thread
link(H,#post{type=thread,id=Tid}=T,#st{board=#board{name=Bn}=B,thread=#post{id=Tid,name_escaped=Tn}}) ->
    H#hookup{body=[?RPL,<<"thread">>],title=[Bn,<<" – "/utf8>>,Tn],
        href=erlach_qs:ml({thread,B,T}),
        onclick=[<<"scrollToPost(\\'">>,spa:id(T),<<"\\');">>]};
% link to post in self thread
link(H,#post{type=post,feed_id={post,Tid},urn=Urn}=P,#st{board=#board{name=Bn}=B,thread=#post{id=Tid,name_escaped=Tn}=T}) ->
    H#hookup{body=[?RPL,Urn],title=[Bn,<<" – "/utf8>>,Tn,<<" – "/utf8>>,Urn],
        href=erlach_qs:ml({post,B,T,P}),
        onclick=[<<"scrollToPost(\\'">>,spa:id(P),<<"\\');">>]};
% link to external thread
link(H,#post{type=thread,feed_id={thread,Bid},name_escaped=Tn}=T,#st{}) ->
    {ok,#board{name=Bn}=B}=kvs:get(board,Bid),
    Href=erlach_qs:ml({thread,B,T}),
    H#hookup{body=[?RPL,Href],title=[Bn,<<" – "/utf8>>,Tn],
        href=Href,postback=(erlach_qs:mp({thread,B,T}))#postback{route_option=highlight} };
% link to external post
link(H,#post{type=post,id=Pid,feed_id={post,Tid},urn=Urn}=P,#st{}) ->
    {ok,#post{type=thread,feed_id={thread,Bid},name_escaped=Tn}=T}=kvs:get(post,Tid),
    {ok,#board{name=Bn}=B}=kvs:get(board,Bid),
    Href=erlach_qs:ml({post,B,T,P}),
    H#hookup{body=[?RPL,Href],title=[Bn,<<" – "/utf8>>,Tn,<<" – "/utf8>>,Urn],
        href=Href,postback=erlach_qs:mp({post,B,T,P}) }.
            

partial(#view{queue=Q,partial=true}=V) ->
    C=wf:config(erlach,partial_count,50),
    T=wf:config(erlach,partial_timeout,10),
    {Instant,Remaining}=lists:split(case length(Q) of L when L > C -> C; L -> L end,Q),
    erlang:send_after(T,self(),{server,V#view{queue=Remaining}}),
    Instant.

       
       