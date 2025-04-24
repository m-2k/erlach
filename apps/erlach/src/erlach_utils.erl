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

post_id(Pid) when is_integer(Pid) -> "e-"++spa_utils:hash({post,Pid});
post_id(#post{id=Pid}) -> post_id(Pid);
post_id(#attachment{post=Pid}) when is_integer(Pid) -> post_id(Pid);
post_id(#attachment{thread=Tid}) when is_integer(Tid) -> post_id(Tid).

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
            Start2=case is_integer(Start) of false -> element(#container.top,C); true -> Start end,
            Count2=case is_integer(Count) of false -> element(#container.count,C); true -> Count end,
            Direction2=case Direction of next -> #iterator.next; _ -> #iterator.prev end,
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
    
cut(Bin,Limit) ->
    Chars=unicode:characters_to_list(Bin,utf8),
    case length(Chars) > Limit of
        true -> <<(unicode:characters_to_binary(lists:sublist(Chars,Limit),utf8))/binary,"…"/utf8>>;
        false -> Bin
    end.