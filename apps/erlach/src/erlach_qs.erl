-module(erlach_qs).
-author('Andy').
-compile(export_all).

-include("erlach.hrl").

urn(#board{urn=Urn}) -> Urn;
urn(#post{urn=Urn}) -> Urn.

% make link
ml(main) -> <<"/">>;
ml(about) -> <<"/about">>;
ml({stream,B}) -> <<"/stream/",(urn(B))/binary>>;
ml({board,B}) -> <<"/",(urn(B))/binary>>;
ml({thread,B,T}) -> <<"/",(urn(B))/binary,"/",(urn(T))/binary>>;
ml({post,B,T,P}) -> <<"/",(urn(B))/binary,"/",(urn(T))/binary,"/",(urn(P))/binary>>;
ml(_) -> <<"/">>.

% make postback
mp(main) -> #postback{action=view,query=#query{ }};
mp(about) -> #postback{action=view,query=#query{q1= <<"about">>}};
mp({stream,B}) -> #postback{action=view,query=#query{q1= <<"stream">>,q2=urn(B)}};
mp({board,B}) -> #postback{action=view,query=#query{q1=urn(B)}};
mp({thread,B,T}) -> #postback{action=view,query=#query{q1=urn(B),q2=urn(T)}};
mp({post,B,T,P}) -> #postback{action=view,query=#query{q1=urn(B),q2=urn(T),q3=urn(P)}}.

is_in_thread_postback(#postback{query=#query{q1= <<"stream">>}}) -> false;
is_in_thread_postback(#postback{query=#query{q2=?UNDEF}}) -> false;
is_in_thread_postback(#postback{query=#query{q2= <<>>}}) -> false;
is_in_thread_postback(#postback{query=#query{q2=UrnT}}) ->
    case spa:st() of
        #st{thread=#post{type=thread,urn=UrnT}} -> true;
        _ -> false
    end.

id_to_urn(Id) -> list_to_binary(string:to_lower(integer_to_list(Id,36))).
urn_to_id(Urn) -> list_to_integer(binary_to_list(Urn),36).


postback_to_path(#postback{action=view,query=#query{}=Q}) -> query_to_path(Q).
query_to_path(#query{q1=Q1,q2=Q2}) ->
    Path=lists:foldl(fun
        (?UNDEF,P) -> P;
        (Bin,P) -> <<P/binary,"/",Bin/binary>>
        end,<<>>,[Q1,Q2]),
    case Path of <<>> -> <<"/">>; _ -> Path end.

state_to_query(#st{route=#route{level=?UNDEF},board=?UNDEF}=S) ->
    Render=state_to_render(S),
    #query{q1=Render:urn()};
state_to_query(#st{route=#route{level=Level},board=?UNDEF}=S) ->
    Render=state_to_render(S),
    #query{q1=Render:urn(),q2=wf:to_binary(Level)};
state_to_query(#st{route=#route{level= <<"stream">>=L},board=B}=S) ->
    #query{q1=L,q2=urn(B)};
state_to_query(#st{board=B,thread=T}) ->
    [Qt,Qb]=lists:foldl(fun
        (?UNDEF,QueryList) -> [?UNDEF|QueryList];
        (Record,QueryList) -> Urn=urn(Record), [Urn|QueryList]
        end,[],[B,T]),
    #query{q1=Qb,q2=Qt}.

state_to_render(#st{route=#route{render=?UNDEF,module=Module}}) -> Module;
state_to_render(#st{route=#route{render=Render}}) -> Render.

history_init() -> history_send(true).
history_update() -> history_send(true).
history_push() -> history_send(false).
history_send(IsReplace) when is_boolean(IsReplace) ->
    State=spa:st(),
    Query=state_to_query(State),
    Secret=n2o_secret:pickle(Query),
    Render=state_to_render(State),
    Title=Render:title(State),
    Path=query_to_path(Query),
    Wire=wf:to_list(list_to_binary([<<"push_state(">>,
        wf:to_binary(IsReplace),<<",'">>,Secret,<<"','">>,Title,<<"','">>,Path,<<"');">>])),
    wf:info(?M,"History: ~p",[Wire]),
    wf:wire(Wire).