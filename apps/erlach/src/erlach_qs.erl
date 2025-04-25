-module(erlach_qs).
-author('Andy').
-compile(export_all).

-include("erlach.hrl").

urn(#board{urn=Urn}) -> Urn;
urn(#post{urn=Urn}) -> Urn;
urn(Id) when is_integer(Id) -> id_to_urn(Id);
urn(Urn) when is_binary(Urn) -> Urn.

% make link
ml(main) -> <<"/">>;
ml(about) -> <<"/about">>;
ml(signin) -> <<"/signin">>;
ml(join) -> <<"/join">>;
ml({stream,B}) -> <<"/stream/",(urn(B))/binary>>;
ml({board,B}) -> <<"/",(urn(B))/binary>>;
ml({thread,B,T}) -> <<"/",(urn(B))/binary,"/",(urn(T))/binary>>;
ml({post,B,T,P}) -> <<"/",(urn(B))/binary,"/",(urn(T))/binary,"/",(urn(P))/binary>>;
ml(_) -> <<"/">>.

% make postback
mp(main) -> #postback{action=view,query=#query{ }};
mp(about) -> #postback{action=view,query=#query{q1= <<"about">>}};
mp(signin) -> #postback{action=view,query=#query{q1= <<"signin">>}};
mp({signin,L}) -> #postback{action=view,query=#query{q1= <<"signin">>,q2=wf:to_binary(L)}};
mp(join) -> #postback{action=view,query=#query{q1= <<"join">>}};
% mp({join,L}) -> #postback{action=view,query=#query{q1= <<"join">>,q2=wf:to_binary(L)}};
mp({stream,B}) -> #postback{action=view,query=#query{q1= <<"stream">>,q2=urn(B)}};
mp({board,B}) -> #postback{action=view,query=#query{q1=urn(B)}};
mp({thread,B,T}) -> #postback{action=view,query=#query{q1=urn(B),q2=urn(T)}};
mp({post,B,T,P}) -> #postback{action=view,query=#query{q1=urn(B),q2=urn(T),q3=urn(P)}}.

up_state(X) -> (mp(X))#postback{route_option=state_update_only}.

is_in_thread_postback(#postback{query=#query{q1= <<"search">>}}) -> false;
is_in_thread_postback(#postback{query=#query{q1= <<"stream">>}}) -> false;
is_in_thread_postback(#postback{query=#query{q1=?URI_SERVICES,q3=Q3}}) -> is_in_thread_postback(#postback{query=#query{q2=Q3}});
is_in_thread_postback(#postback{query=#query{q2=?UNDEF}}) -> false;
is_in_thread_postback(#postback{query=#query{q2= <<>>}}) -> false;
is_in_thread_postback(#postback{query=#query{q2=UrnT}}) ->
    case spa:st() of
        #st{action=search} -> false;
        #st{thread=#post{type=thread,urn=UrnT}}=S -> true;
        _ -> false
    end.

id_to_urn(Id) -> list_to_binary(string:to_lower(integer_to_list(Id,36))).
urn_to_id(Urn) -> list_to_integer(binary_to_list(Urn),36).


postback_to_path(#postback{action=view,query=#query{}=Q}) -> query_to_path(Q).
query_to_path(#query{q1=Q1,q2=Q2,q3=Q3,q4=Q4}) ->
    GenPath=fun(L) -> lists:foldl(fun(?UNDEF,P) -> P; (Bin,P) -> <<P/binary,"/",Bin/binary>> end,<<>>,L) end,
    case Q1 of
        ?URI_SERVICES -> <<"/",(?URI_SERVICES)/binary,(GenPath([Q2,Q3,Q4]))/binary>>;
        _ -> case GenPath([Q1,Q2,Q3]) of <<>> -> <<"/">>; Url -> Url end
    end.

state_to_query(?UNDEF) -> #query{};
state_to_query(#st{action=search,search=Q}) -> #query{q1= <<"search">>,q2=Q};
state_to_query(#st{route=#route{render=R,level=L}}=S) ->
    wf:info(?M,"state_to_query: ~p",[1]),
    case R:urn() of
        ?URN_PAGE_DYNAMIC -> state_to_query_dyn(S);
        PageUrn ->
            #query{q1=PageUrn,q2=L}
    end.

state_to_query_dyn(#st{board=B,thread=T,post=P,services=Svc}) ->
    wf:info(?M,"state_to_query_dyn: ~p",[2]),
    [Qp,Qt,Qb]=lists:foldl(fun
        (?UNDEF,QueryList) -> [?UNDEF|QueryList];
        (Record,QueryList) -> Urn=urn(Record), [Urn|QueryList]
        end,[],[B,T,P]),
    case Svc of
        comments -> #query{q1=?URI_SERVICES,q2=Qb,q3=Qt,q4=Qp};
        false -> #query{q1=Qb,q2=Qt,q3=Qp}
    end.


state_to_render(#st{route=#route{render=?UNDEF,module=Module}}) -> Module;
state_to_render(#st{route=#route{render=Render}}) -> Render.

title(Render,#st{action=search}=S) -> ?SEARCH:title(S);
title(Render,#st{}=S) -> Render:title(S).

history_init() -> history_send(true).
history_update() -> history_send(true).
history_push() -> history_send(false).
history_send(IsReplace) when is_boolean(IsReplace) ->
    State=spa:st(),
    Query=state_to_query(State),
    Secret=n2o_secret:pickle(Query),
    Render=state_to_render(State),
    Title=wf:jse(title(Render,State)),
    wf:info(?M,"History Query: ~p",[Query]),
    Path=query_to_path(Query),
    wf:info(?M,"History Path: ~p",[Path]),
    {Board,Thread}=case State of #st{board=B,thread=T} -> {B,T}; _ -> {?UNDEF,?UNDEF} end,
    Burn=case Board of #board{} -> urn(Board); _ -> [] end,
    Turn=case Thread of #post{} -> urn(Thread); _ -> [] end,
    wf:info(?M,"History bUrn: ~p tUrn: ~p",[Burn,Turn]),
    Wire=wf:to_list(list_to_binary(["push_state(",
        wf:to_binary(IsReplace),",'",Secret,"','",Title,"','",Path,"','",Burn,"','",Turn,"');"])),
    wf:info(?M,"History: ~ts",[Wire]),
    wf:wire(Wire).