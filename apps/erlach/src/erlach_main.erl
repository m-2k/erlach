-module(erlach_main).
-compile(export_all).
-author('andy').
-include_lib("n2o/include/wf.hrl").
-include_lib("nitro/include/nitro.hrl").

-include_lib("erlach_db/include/erlach_db.hrl").
-include("erlach.hrl").

title(#st{}) -> <<"Erlach Imageboards Service">>.
urn() -> ?UNDEF.

init(#route{}=Route) ->
    wf:info(?M,"init",[]),
    {ok,#st{route=Route,action=view}}.
terminate() -> wf:info(?M,"TERMINATE ~p",[self()]).

render(content=Panel,#st{}=S) ->
    #panel{id=Panel,class= <<"board-list">>,
        body=lists:foldl(fun(#group{id=Gid,name=Gname,desc=Gdesc},Acc) ->
            BoardList=kvs:entries(kvs:get(feed,{board,Gid}),board,undefined),
            GroupButton=#panel{class= <<"inline b black">>,title=Gdesc,body=Gname},
            BoardButtonList=lists:foldl(fun(B,Acc) ->
                BoardPanel=#panel{class= <<"inline">>,body=[
                    #hookup{class= <<"b">>,title=B#board.name,
                        href=erlach_qs:ml({board,B}),
                        body=[$/,B#board.urn,$/],
                        postback=erlach_qs:mp({board,B})
                        }]},
                [BoardPanel|Acc] end,[],BoardList),
            [#panel{class= <<"group">>,body=[GroupButton|BoardButtonList]}|Acc]
    end,[],kvs:entries(kvs:get(feed,group),group,undefined))}.

event(#render_event{event=sus}) -> wf:info(?M,"sus INIT ~p",[self()]),
    wf:update(content,#panel{id=content,body=
        #button{body= <<"SuS">>,postback=#render_event{event=nus}}});

event(Unknown) -> ?EVENT_ROUTER:unknown(?M,Unknown).