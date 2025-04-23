-module(spa).
-author('Andy').

-compile(export_all).
-include("spa.hrl").
-include_lib("nitro/include/nitro.hrl").

%%% State
st() -> erlang:get(?STATE).
st(S) -> erlang:put(?STATE,S),S.

%%% Temp tag ids generator
temp_id() -> [$e,$-|integer_to_list(abs(erlang:unique_integer() rem 1000000000))].
temp_id(Count) -> [ temp_id() || _ <- lists:seq(1,Count) ].

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
id_storage_list(S) -> [ E || {K,_}=E <- erlang:get(), case K of {S,_} -> true; _ -> false end ].
id_storage_erase(E,S) -> erlang:erase({S,id_storage_key(E)}).
id_storage_erase(S) -> [ erlang:erase(K) || {K,_} <- erlang:get(), case K of {S,_} -> true; _ -> false end ], ok.

%%% Updating client forms
% Render/2
update(_M,?UNDEF) -> skip;
update(M,R) when is_tuple(R) -> wf:update(id(R),M:render({element(1,R),R},st())); %% TODO:
update(M,Panel) -> wf:update(Panel,M:render(Panel,st())).

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
