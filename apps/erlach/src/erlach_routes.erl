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
    Route=route(#query{q1=Q1,q2=Q2,q3=Q3}),
    wf:info(?MODULE,"ROUTE: ~p ~p ~p ~p",[wf:path(Req),Q1,Q2,Q3]),
    {ok, State, Ctx#cx{path=Route,module=Route#route.module}}.

route(#query{q1=Q1,q2=Q2}=Q) ->
    R=route(Q1,Q2),
    R#route{query=Q}.


% /br/                    -> board
% /br/thread              -> board:blog
% /br/:category-1         -> board&cat
% /br/AB68/                -> thread

% /:BOARD/(:CAT|:TYPE|:NEW|:THREAD)/(:CAT|:THREAD)/

route(?UNDEF,?UNDEF) -> #route{module=?SPA,render=erlach_main};
route(<<"about">>=Level,?UNDEF) -> #route{module=?SPA,render=erlach_about,level= <<"about">>};
route(<<"stream">>,Board) -> #route{module=?SPA,render=erlach_stream,board=Board,level= <<"stream">>};
route(Board,?UNDEF) -> #route{module=?SPA,render=erlach_board,board=Board};
route(Board,Thread) -> #route{module=?SPA,render=erlach_thread,board=Board,thread=Thread}.