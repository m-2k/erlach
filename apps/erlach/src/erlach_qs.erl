-module(erlach_qs).
-author('andy').
-compile(export_all).

-include_lib("erlach_db/include/erlach_db.hrl").
-include("erlach.hrl").


urn(#board{urn=Urn}) -> Urn;
urn(#thread{urn=Urn}) -> Urn.

% % make link
ml(main) -> <<"/">>;
ml({board,B}) -> <<"/",(urn(B))/binary>>;
ml({thread,B,T}) -> <<"/",(urn(B))/binary,"/",(urn(T))/binary>>;
ml(_) -> <<"/">>.

% % make postback
mp(main) -> #postback{action=view,query=#query{ }};
mp({board,B}) -> #postback{action=view,query=#query{q1=urn(B)}};
mp({thread,B,T}) -> #postback{action=view,query=#query{q1=urn(B),q2=urn(T)}}.

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
    State=erlang:get(state),
    Query=state_to_query(State),
    Secret=n2o_secret:pickle(Query),
    Render=state_to_render(State),
    Title=Render:title(State),
    Path=query_to_path(Query),
    Wire=wf:to_list(list_to_binary([<<"push_state(">>,
        wf:to_binary(IsReplace),<<",'">>,Secret,<<"','">>,Title,<<"','">>,Path,<<"');">>])),
    wf:info(?M,"History: ~p",[Wire]),
    wf:wire(Wire).