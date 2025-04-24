-module(spa_render).
-author('Andy').

-compile(export_all).
-include("spa.hrl").
-include("spa_db.hrl").
-include_lib("nitro/include/nitro.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%                                                     %%%%%
%%%%%         CODE WARNING: ABSTRACT RECORDS MODE         %%%%%
%%%%%                                                     %%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% Default user scope-id function
si_key(Scope,?UNDEF) -> Scope;
si_key(?UNDEF,E) -> E;
si_key(Scope, E) -> {Scope,element(2,E)}.

% scoped container/element ids
si(Scope,E) -> spa:id(si_key(Scope,E)).
si_exist(Scope,E) -> spa:id_exist(si_key(Scope,E)).
si_erase(Scope,E) -> spa:id_erase(si_key(Scope,E)).


update(E) -> update(spa:render(spa:st()),E).

update(R,E) when is_tuple(E)       -> update_fun(spa:ei(?UNDEF,E),                R:render(E,      spa:st()));
update(R,Panel)                    -> update_fun(Panel,                           R:render(Panel,  spa:st())).

update(R,E,    H) when is_tuple(E) -> update_fun(spa:ei(?RM:get(#hes.scope,H),E), R:render(E,    H,spa:st()));
update(R,Panel,H)                  -> update_fun(Panel,                           R:render(Panel,H,spa:st())).

update_fun(Target,Elements) -> wf:wire(#replace{target=Target,elements=Elements}).


buttons(#compose{panel=content,guard=GuardFun}=Compose) ->
    case GuardFun(Compose) of
        #compose{element=E,scope=Sc,forms=Fm,state=S,into=In,body=B,button=ButtonFun}=C ->
            R=spa:render(S),
            [Target,Id,Feed,Del]=?RM:get_many([1,2,#db_element.feed_id,#db_element.deleted],E),
            wf:info(?M,"ACTION: ~p ~p ~p",[spa:action(S),Id,E]),
            PbList=case spa:action(S) of
                {create,Target,Panel,Sc} when Id =:= ?UNDEF -> [
                    #cancel{render=R,target=Target,panel=Panel,scope=Sc},
                    #add{render=R,target=Target,panel=Panel,feed=Feed,scope=Sc,forms=Fm,into=In} ];
                {edit,Target,Id,Sc} -> [
                    #update{render=R,target=Target,scope=Sc,id=Id},
                    #put{render=R,target=Target,id=Id,scope=Sc,forms=Fm} ];
                _ -> [
                    #edit{render=R,target=Target,scope=Sc,id=Id},
                    #delete{render=R,target=Target,id=Id,scope=Sc,value=not Del} ]
            end,
            Sr=spa:panels(Fm),
            [ [ ButtonFun(Pb,Sr) || Pb <- PbList ], B ];
        _ -> []
    end;
buttons(#compose{panel=controls,guard=GuardFun}=Compose) ->
    case GuardFun(Compose) of
        #compose{element=E,scope=Sc,state=S,button=ButtonFun,body=B}=C ->
            case spa:action(S) of
                view -> [ ButtonFun(#create{render=spa:render(S),target=?RM:name(E),scope=Sc,feed=?RM:get(#db_element.feed_id,E)},[]), B ];
                _ -> B
            end;
        _ -> []
    end.
    
button(Pb,Sr) -> #button{body=wf:to_binary(element(1,Pb)),postback=Pb,source=Sr}.
