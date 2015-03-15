-module(qs).
-compile(export_all).

-include("erlach.hrl").
-include_lib("db/include/board.hrl").

% /g/                     -> board
% /g/blog                 -> board:blog
% /g/blog/AB68            -> thread:blog
% /g/blog/AB68/category-1 -> thread:blog&cat
% /g/AB68/                -> thread
% /g/AB68/category-1      -> thread&cat
% /g/new
% /g/blog/new


% {"/[:board/[:type/[:thread/[:category]]]]", [{type, function, fun(<<"blog">>) -> true; (_) -> false end}], n2o_cowboy, []},
% {"/[:board/[:new]]", [{new, function, fun(<<"new">>) -> true; (_) -> false end}], n2o_cowboy, []},
% {"/[:board/[:type/[:new]]]", [{type, function, fun(<<"blog">>) -> true; (_) -> false end},
%     {new, function, fun(<<"new">>) -> true; (_) -> false end}], n2o_cowboy, []},
% {"/[:board/[:thread/[:category]]]", n2o_cowboy, []}

parse_qs(Req) ->
    wf:info(?MODULE,"TEST", []),
    {Bindings,_}=cowboy_req:bindings(Req),
    
    lists:foldl(fun({Key,Value}, R) ->
        case Key of
            board    -> R#route{board=Value};
            type     -> R#route{type=Value};
            thread   -> R#route{thread=Value};
            category -> R#route{category=Value};
            new      -> R#route{new=Value}
        end end,#route{},Bindings).

% make link
ml({thread,create,Uri})  -> "/"++wf:to_list(Uri)++"/new";
% ml({thread,blog,create,Uri})  -> "/"++wf:to_list(Uri)++"/blog/new";
ml({thread,Uri,Id})  -> "/"++wf:to_list(Uri)++"/"++erlang:integer_to_list(Id,?IDS_BASE);
ml({board,Uri})      -> "/"++wf:to_list(Uri);
ml({board,blog,Uri}) -> "/"++wf:to_list(Uri)++"/blog".

init() ->
    Proplist=lists:foldl(fun(#board{id=Id,uri=Uri},Acc) -> [{wf:to_binary(Uri),Id}|Acc] end,[],kvs:all(board)),
    ets:insert(globals,{board_uri,Proplist}), Proplist.

board_uri_to_id(Uri) ->
    proplists:get_value(Uri, ets:lookup_element(globals,board_uri,2), undefined).