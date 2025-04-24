-module(spa_event).
-author('Andy').

-compile(export_all).

-include("spa.hrl").
-include("spa_db.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%                                                     %%%%%
%%%%%         CODE WARNING: ABSTRACT RECORDS MODE         %%%%%
%%%%%                                                     %%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%                                                     %%%%%
%%%%%    API INFO: Use store/1 for manage data storing    %%%%%
%%%%%                                                     %%%%%
%%%%%      Render:manage(AddEvent) -> Record | default    %%%%%
%%%%%        Render = module()                            %%%%%
%%%%%        AddEvent = #add{}                            %%%%%
%%%%%        Record -> tuple()                            %%%%%
%%%%%                                                     %%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%                                                     %%%%%
%%%%%   API INFO: Use is_can_view/1 for access checking   %%%%%
%%%%%                                                     %%%%%
%%%%%      Render:is_visible(E) -> boolean()              %%%%%
%%%%%        Render = module()                            %%%%%
%%%%%        E = #db_element{}                            %%%%%
%%%%%                                                     %%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%
%%% TODO: add guards to events and remove (custom) guard from tuples
%%%

is_visible(R,E) ->
    case erlang:function_exported(R,is_visible,1) of
        true -> R:is_visible(E);
        false -> false
    end.

hes(Sc) -> ?RM:new(hes,[{#hes.scope,Sc}]). 

event(#pubsub{action=update,render=R,data=E,scope=Sc}=Ev) ->
    IsContain=spa:ei_exist(Sc,E),
    case {IsContain,is_visible(R,E)} of
        {true,true} -> spa:update(R,E,hes(Sc));
        {true,false} -> wf:remove(spa:ei_erase(Sc,E));
        {false,true} -> wf:insert_bottom(spa:ci(Sc),R:render(E,hes(Sc),spa:st())); % TODO: refactor
        _ -> wf:info(?M,"PubSub update skip ~p ~p",[self(),Ev]), skip
    end;
event(#pubsub{action=insert,target=_Tg,render=R,data=E,into=In,scope=Sc}) ->
    case is_visible(R,E) of
        true ->
            Action=case In of top -> insert_top; _ -> insert_bottom end,
            wf:Action(spa:ci(Sc),R:render(E,hes(Sc),spa:st()));
        false -> skip
    end;


event(#edit{target=Tg,render=R,scope=Sc,id=Id}) ->
    S=spa:st(),
    S3=case spa:action(S) of % canceling other edition
        {create,_OtherTg,Panel,_OtherSc} -> wf:remove(Panel),S;
        {edit,OtherTg,OtherId,OtherSc} ->
            {ok,OtherE}=kvs:get(OtherTg,OtherId),
            S2=spa:action(view,S),
            spa:update(R,OtherE,hes(OtherSc)),
            S2;
        _ -> S
    end,
    {ok,E}=kvs:get(Tg,Id),
    _S4=spa:action({edit,Tg,Id,Sc},S3),
    spa:update(R,controls),
    spa:update(R,E,hes(Sc));
event(#cancel{target=_Tg,render=R,panel=Panel}) ->
    spa:action(view,spa:st()),
    spa:update(R,controls),
    wf:remove(Panel);
event(#update{target=Tg,render=R,scope=Sc,id=Id}=Ev) ->
    wf:info(?M,"event: ~p",[Ev]),
    {ok,E}=kvs:get(Tg,Id),
    spa:action(view,spa:st()),
    spa:update(R,controls),
    spa:update(R,E,hes(Sc));
event(#create{target=Tg,render=R,scope=Sc,feed=F}) ->
    E=?RM:new(Tg,[{#db_element.feed_id,F}]),
    wf:warning(?M,"ADD Sc:~p ~p",[Sc,spa:ci(Sc)]),
    Panel=spa:ei(Sc,E),
    spa:action({create,Tg,Panel,Sc},spa:st()),
    spa:update(R,controls),
    wf:insert_top(spa:ci(Sc),R:render(E,hes(Sc),spa:st()));
event(#delete{target=Tg,render=R,id=_Id,scope=Sc,value=_Boolean}=De)->
    E3=case erlang:function_exported(R,store,1) of
        true -> case R:store(De) of default -> store(De); E2 -> E2 end;
        false -> store(De)
    end,
    publish(#pubsub{render=R,target=Tg,action=update,scope=Sc,data=E3});
event(#put{target=Tg,render=R,id=_Id,scope=Sc,forms=_Fm}=Pe) ->
    E3=case erlang:function_exported(R,store,1) of
        true -> case R:store(Pe) of default -> store(Pe); E2 -> E2 end;
        false -> store(Pe)
    end,
    spa:action(view,spa:st()),
    spa:update(R,controls),
    publish(#pubsub{render=R,target=Tg,action=update,scope=Sc,data=E3});
event(#add{target=Tg,render=R,panel=Panel,scope=Sc,into=In}=Ae) ->
    E3=case erlang:function_exported(R,store,1) of
        true -> case R:store(Ae) of default -> store(Ae); E2 -> E2 end;
        false -> store(Ae)
    end,
    wf:remove(Panel),
    spa:action(view,spa:st()),
    spa:update(R,controls),
    publish(#pubsub{render=R,target=Tg,action=insert,data=E3,into=In,scope=Sc}).

%%%
%%% Store
%%%
store(#add{target=Tg,feed=F,forms=Fm}) ->
    Init=[{id,kvs:next_id(atom_to_list(Tg),1)},{created,erlang:timestamp()},{feed_id,F},{deleted,true}],
    E=?RM:new(Tg,Init),
    E2=spa:apply_forms(spa:extract_forms(Fm),E),
    {ok,E3}=spa_feeds:call(append,E2),
    E3;
store(#put{target=Tg,id=Id,forms=Fm}) ->
    {ok,E}=kvs:get(Tg,Id),
    Efm=spa:extract_forms(Fm),
    {ok,E3}=spa_feeds:call(update,E,fun(E2) -> {ok,spa:apply_forms(Efm,E2)} end),
    E3;
store(#delete{target=Tg,id=Id,value=Boolean})->
    {ok,E}=kvs:get(Tg,Id),
    {ok,E3}=spa_feeds:call(update,E,fun(E2) -> {ok,?RM:set(#db_element.deleted,E2,Boolean)} end),
    E3.



publish(#pubsub{data=E}=M) ->
    Chan=?RM:channel(E),
    wf:info(?M,"Publishing to channel: ~p ~p",[Chan,M]),
    wf:send(Chan,{server,M}).
