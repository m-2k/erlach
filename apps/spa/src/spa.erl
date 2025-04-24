-module(spa).
-author('Andy').
-behaviour(application).
-export([start/2, stop/1, init/1]).
-compile(export_all).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%                                                      %%%%%
%%%%%  CODE WARNING: ABSTRACT MODE FOR OVERLOADED RECORDS  %%%%%
%%%%%                                                      %%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-include("spa.hrl").
-include_lib("nitro/include/nitro.hrl").

start(_,_) -> supervisor:start_link({local,?MODULE},?MODULE,[]).
stop(_) -> ok.
init([]) -> {ok, {{one_for_one,5,10}, []} }.

%%% Option helper for #hes{}
option(Key,From) -> option(Key,From,false).

option(Key,Hes,Default) when ?REC(Hes,hes) -> spa_utils:option(Key,element(#hes.option,Hes),Default);
option(Key,From,Default) -> spa_utils:option(Key,From,Default).
setoption(Key,Value,Hes) when ?REC(Hes,hes) -> setelement(#hes.option,Hes,spa_utils:setoption(Key,Value,element(#hes.option,Hes)));
setoption(Key,Value,To) -> spa_utils:setoption(Key,Value,To).


redirect(#postback{}=P) -> self() ! {server,P}.

%%% State
st() -> erlang:get(?STATE).
st(S) -> erlang:put(?STATE,S),S.

render(R) when ?REC(R,route) -> element(#route.render,R);
render(S) when ?REC(S,st) -> render(element(#st.route,S)).

level(S) -> element(#route.level,element(#st.route,S)).
level(S,Level) -> st(setelement(#st.route,S,setelement(#route.level,element(#st.route,S),Level))).

action(State) -> element(#st.action,State).
action(Value,State) -> S2=setelement(#st.action,State,Value), st(S2).

% for injecting apply_forms/2 into another process without cx
extract_forms(Forms) -> [ {Field,wf:q(Panel)} || {Field,Panel} <- Forms ].
apply_forms(ExtractedForms,R) ->
    lists:foldl(fun
        ({{prop,P,F},V},AR) ->
            {K,V2}=normalize(F,V),
            V0=?RM:get(K,AR),
            V3=spa_utils:prop_put({P,V2},V0),
            ?RM:set(K,AR,V3);
        ({F,V},AR) ->
            {K,V2}=normalize(F,V),
            ?RM:set(K,AR,V2)
        end,R,ExtractedForms).

normalize({radio,K,X},V) -> {K,element(wf:to_integer(V),list_to_tuple(X))};
normalize({integer,K},V) -> {K,wf:to_integer(V)};
normalize({binary,K},V)  -> {K,wf:to_binary(V)};
normalize({bool,K},V)    -> {K,V =:= <<"on">>};
normalize(K,V)           -> {K,V}.

%%% Render IF helper
rif({X,X},V) -> V;
rif(true,V) -> V;
rif(_,_) -> [].


%%% Temp tag ids generator

temp_id() -> [$(|spa_utils:hex64(binary:encode_unsigned(erlang:unique_integer([positive])))]++[$)].
temp_id(Count) -> [ temp_id() || _ <- lists:seq(1,Count) ].

forms(Fields) -> FS=[ {F,temp_id()} || F <- Fields ], {FS,panels(FS)}.
panels(Forms) -> [ Panel || {_,Panel} <- Forms ].

update(E) -> spa_render:update(E).
update(R,E) -> spa_render:update(R,E).
update(R,E,H) -> spa_render:update(R,E,H).

% scoped container id
ci(Sc) -> spa_render:si(Sc,?UNDEF).
ci_exist(Sc) -> spa_render:si_exist(Sc,?UNDEF).
ci_erase(Sc,_E) -> spa_render:si_erase(Sc,?UNDEF).

% scoped element id
ei(Sc,E) -> spa_render:si(Sc,E).
ei_exist(Sc,E) -> spa_render:si_exist(Sc,E).
ei_erase(Sc,E) -> spa_render:si_erase(Sc,E).

%%% Page timelife
id(Key) -> id_storage(Key,temp_id).
id_exist(Key) -> id_storage_exist(Key,temp_id).
id_list() -> id_storage_list(temp_id).
id_erase() -> id_storage_erase(temp_id).
id_erase(Key) -> id_storage_erase(Key,temp_id).

%%% WS session timelife
eternal_id(Key) -> id_storage(Key,eternal_id).
eternal_id_exist(Key) -> id_storage_exist(Key,eternal_id).
eternal_id_erase() -> id_storage_erase(eternal_id).
eternal_id_erase(Key) -> id_storage_erase(Key,eternal_id).


%%% Private block (E: element, S: storage)
id_storage_key({Scope,Id}) -> {Scope,Id};
id_storage_key(R) when is_tuple(R) -> {element(1,R),element(2,R)};
id_storage_key(E) -> {panel,E}.

id_storage(E,S) ->
    K=id_storage_key(E),
    case erlang:get({S,K}) of
        ?UNDEF -> I=temp_id(), erlang:put({S,K},I),I;
        I -> I end.
id_storage_exist(E,S) -> erlang:get({S,id_storage_key(E)}) =/= ?UNDEF.
% warning {shadowed_var,'S',generate}
id_storage_list(S) -> [ E || {{Ss,_},_}=E <- erlang:get(), S =:= Ss ].
id_storage_erase(E,S) -> erlang:erase({S,id_storage_key(E)}).
% warning {shadowed_var,'S',generate}
id_storage_erase(S) -> [ erlang:erase(K) || {{Ss,_}=K,_} <- erlang:get(), S =:= Ss ], ok.

%%%% TODO: replace to maps (temp_id, storages, rst)

%%% Render Accumulator

rac() -> case get(rac) of Rac when is_map(Rac) -> Rac; _ -> #{} end.
rac_write(Rac) when is_map(Rac) -> put(rac,Rac), Rac.
rac(K) -> maps:get(K,rac()).
rac(K,V) -> rac_write(maps:put(K,V,rac())).
rac_exist(K) -> maps:is_key(K,rac()).
rac_erase() -> rac_write(#{}).

add_class(Target,ClassList) -> jq_apply(Target,"classList.add",ClassList).
remove_class(Target,ClassList) -> jq_apply(Target,"classList.remove",ClassList).
jq_apply(Target,Method,ClassList) ->
    Args=[ ["'",C,"'"] || C <- lists:flatten([ClassList]) ],
    #jq{target=Target,method=[Method],args=Args}.

%%% Notifications
info(M) -> notify(M, <<"info">>).
success(M) -> notify(M, <<"succ">>).
warning(M) -> notify(M, <<"warn">>).
error(M) -> notify(M, <<"error">>).
notify(M, Class) ->
    Message=wf:to_binary(M),
    Id=temp_id(),
    wf:insert_top(?NOTIFY_PANEL, #button{id=Id, class=Class,
        onclick="qi(\\'"++Id++"\\').parentNode.removeChild(qi(\\'"++Id++"\\'));",
        body=wf:html_encode(Message)}),
    wf:wire("window.setTimeout(function(){var a=qi('"++Id++"'); if(a){a.onclick();}},"
        ++wf:to_list(size(Message)*100+4000)++");"),[].

console(T,Args) ->
    wf:wire("debug && console.log('"++wf:jse(wf:f(T,Args))++"');").
