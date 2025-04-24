-module(erlach_routes).
-author('Andy').
-include_lib("n2o/include/wf.hrl").
-export([init/2, finish/2, route/1]).
-include("erlach.hrl").

finish(State, Ctx) -> {ok, State, Ctx}.

init(State, Ctx) ->
    Req=Ctx#cx.req,
    {Q1,_}=cowboy_req:binding(q1,Req),
    {Q2,_}=cowboy_req:binding(q2,Req),
    {Q3,_}=cowboy_req:binding(q3,Req),
    {Q4,_}=cowboy_req:binding(q4,Req),
    Route=route(#query{q1=Q1,q2=Q2,q3=Q3,q4=Q4}),
    wf:info(?MODULE,"ROUTE: ~p ~p ~p ~p",[wf:path(Req),Q1,Q2,Q3]),
    {ok, State, Ctx#cx{path=Route,module=Route#route.module}}.

route(#query{q1=Q1,q2=Q2,q3=Q3,q4=Q4}=Q) ->
    R=case Q1 of
        ?URI_SERVICES -> services(Q2,Q3,Q4);
        _ -> route(Q1,Q2,Q3)
    end,
    R#route{query=Q}.

route(?UNDEF,         ?UNDEF, _) -> #route{module=?SPA,render=erlach_main};
route(<<"about">>=L,  ?UNDEF, _) -> #route{module=?SPA,render=erlach_about,             level=L};
route(<<"stream">>=L, Board,  _) -> #route{module=?SPA,render=erlach_stream,board=Board,level=L};
route(Board,          ?UNDEF, _) -> #route{module=?SPA,render=erlach_board, board=Board};
route(Board,          Thread, Post) -> #route{module=?SPA,render=erlach_thread,board=Board,thread=Thread,post=Post}.

services(Board,Thread,Post)  -> #route{module=?SPA,render=erlach_services,board=Board,thread=Thread,post=Post}.