-module(erlach_main).
-compile(export_all).
-author('Andy').

-include("erlach.hrl").

title(#st{}) -> <<"Erlach Imageboards Service">>.
urn() -> ?UNDEF.

init(#route{}=Route) ->
    wf:info(?M,"init",[]),
    {ok,#st{route=Route,action=view}}.
finalize(#st{}) -> ok.
terminate() -> wf:info(?M,"TERMINATE ~p",[self()]).

render(content=Panel,#st{}=S) ->
    #panel{id=Panel,body=#panel{class= <<"board-list">>,
        body=lists:foldl(fun(#party{id=Gid,name=Gname,desc=Gdesc},Acc) ->
            BoardList=kvs:entries(kvs:get(feed,{board,Gid}),board,undefined),
            GroupButton=#panel{class= <<"inline b black inactive">>,title=Gdesc,body=Gname},
            BoardButtonList=lists:foldl(fun(B,Acc) ->
                BoardPanel=#panel{class= <<"inline">>,body=[
                    #hookup{class= <<"b blue">>,title=B#board.name,
                        href=erlach_qs:ml({board,B}),
                        body=[$/,B#board.urn,$/],
                        postback=erlach_qs:mp({board,B})
                        }]},
                [BoardPanel|Acc] end,[],BoardList),
            [#panel{class= <<"group">>,body=[GroupButton|BoardButtonList]}|Acc]
    end,[],kvs:entries(kvs:get(feed,party),party,undefined))}}.

event(#render_event{event=sus}) -> wf:info(?M,"sus INIT ~p",[self()]),
    wf:update(content,#panel{id=content,body=
        #button{body= <<"SuS">>,postback=#render_event{event=nus}}});

event(#view{target=sidebar,option=Visibled,element=Btn}=E) ->
    wf:update(Btn,#hookup{id=Btn,class=case Visibled of true -> [selector,checked]; false -> selector end,body= <<"Notify">>,postback=E#view{option=not Visibled}}),
    wf:wire(wf:f("qi('sidebar').dataset.visibled=~s;",[Visibled])); % data-* for safari supports

event(Unknown) -> ?EVENT_ROUTER:unknown(?M,Unknown).