-module(donate).
-author('andy').
-vsn('0.0.0').

-compile(export_all).
-include_lib("n2o/include/wf.hrl").
-include("erlach.hrl").

main() -> #dtl{file="erlach",app=erlach,bindings=[{body,body()}, {title,<<"Donate">>}]}.
body() ->
    Content = #panel{class= <<"center">>, body=iframe()},
    html:body(Content).

event(init) -> ok;

event(terminate) -> skip;
event(Event) -> guard:shared_event(?MODULE, Event).


iframe() -> <<>>.
% iframe() ->
%     <<"<iframe frameborder='0' allowtransparency='true' scrolling='no' src='https://money.yandex.ru/embed/donate.xml?' width='488' height='201'></iframe>">>.