-module(routes).
-author('Maxim Sokhatsky').
-include_lib("n2o/include/wf.hrl").
-export([init/2, finish/2]).

-include("erlach.hrl").

%% U can use default dynamic routes or define custom static as this
%% Just put needed module name to sys.config:
%% {n2o, [{route,routes}]}
%% Also with dynamic routes u must load all modules before starting Cowboy
%% [code:ensure_loaded(M) || M <- [index, login, ... ]]

finish(State, Ctx) -> {ok, State, Ctx}.
% init(State, Ctx) ->
%     % Path = wf:path(Ctx#cx.req),
%     % wf:info(?MODULE,"ROUTE path: ~p",[Ctx#cx.path]),
%
%     % Route = case erlang:erase(route) of
%     %     undefined ->
%             R = qs:parse_qs(Ctx#cx.req),
%             Module = case route(R#route.board) of
%                 undefined ->
%                     case R#route.new of undefined ->
%                         case R#route.thread of undefined -> board; _ -> thread end;
%                         _ -> thread
%                     end;
%                 M -> M end,
%             R2=R#route{module=Module},
%     %         wf:wire(#transfer{state=[{route,R2}]}),
%     %         R2;
%     %     Exist -> Exist
%     % end,
%     wf:info(?MODULE,"Route: ~p ~p~n",[self(),R2]),
%     {ok, State, Ctx#cx{path=R2,module=R2#route.module}}.

init(State, Ctx) ->
    Req=Ctx#cx.req,
    
    {Bindings,_}=cowboy_req:bindings(Req),
    wf:info(?MODULE,"ROUTE Bindings: ~p",[Bindings]),
    
    {Q1,_}=cowboy_req:binding(q1,Req),
    {Q2,_}=cowboy_req:binding(q2,Req),
    {Q3,_}=cowboy_req:binding(q3,Req),
    
    Route = case static(Q1) of
        undefined -> dinamic(Q1,Q2,Q3);
        Static -> #route{module=Static} end,
    wf:info(?MODULE,"ROUTE: ~p",[Route]),
    {ok, State, Ctx#cx{path=Route,module=Route#route.module}}.

% /g/                     -> board
% /g/blog                 -> board:blog
% /g/blog/new
% /g/new
% /g/:category-1          -> board&cat
% /g/blog/:category-1     -> board:blog&cat
% /g/blog/AB68            -> thread:blog
% /g/AB68/                -> thread



% /:BOARD/(:CAT|:TYPE|:NEW|:THREAD)/(:CAT|:THREAD)/

dinamic(Board,undefined,undefined)               -> #route{module=board,board=Board,type=thread};
dinamic(Board,<<"blog">>,undefined)              -> #route{module=board,board=Board,type=blog};
dinamic(Board,<<"blog">>,<<"new">>)              -> #route{module=thread,board=Board,type=blog};
dinamic(Board,<<"new">>,undefined)               -> #route{module=thread,board=Board,type=thread};
dinamic(Board,<<$:,Category/binary>>,undefined)  -> #route{module=board,board=Board,type=thread,category=Category};
dinamic(Board,<<"blog">>,<<$:,Category/binary>>) -> #route{module=board,board=Board,type=blog,category=Category};
dinamic(Board,<<"blog">>,Thread)                 -> #route{module=thread,board=Board,type=blog,thread=Thread};
dinamic(Board,Thread,undefined)                  -> #route{module=thread,board=Board,type=thread,thread=Thread};
dinamic(_,_,_) -> root.
    

static(undefined)         -> root;
static(<<"privacy">>)     -> privacy;
static(<<"donate">>)      -> donate;
static(<<"profile">>)     -> profile;
static(<<"favicon.ico">>) -> static_file;
static(_) -> undefined.
