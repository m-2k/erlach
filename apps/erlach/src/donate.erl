-module(donate).
-vsn('0.0.0').

-compile(export_all).
-include_lib("n2o/include/wf.hrl").
% -include_lib("kvs/include/kvs.hrl").
% -include_lib("kvs/include/feed.hrl").
% -include_lib("db/include/post.hrl").
% -include_lib("db/include/attachment.hrl").
%
% -include_lib("db/include/board.hrl").
% -include_lib("db/include/user.hrl").
-include("erlach.hrl").

% -ifndef(SESSION).
% -define(SESSION, (wf:config(n2o,session,erlach_session))).
% -endif.

main() -> #dtl{file="erlach",app=erlach,bindings=[{body,body()}, {theme,<<"glassy donate">>}, {title,<<"Donate">>}]}.
body() ->
    Content = #panel{class= <<"center">>, body=iframe()},
    html:body(Content).

event(init) -> ok;

event(terminate) -> skip;
event(Event) -> guard:shared_event(?MODULE, Event).


iframe() ->
    <<"<iframe frameborder='0' allowtransparency='true' scrolling='no' src='https://money.yandex.ru/embed/donate.xml?account=41001725234314&quickpay=donate&payment-type-choice=on&default-sum=290&targets=%D0%A1%D0%B1%D0%BE%D1%80+%D1%81%D1%80%D0%B5%D0%B4%D1%81%D1%82%D0%B2+%D0%BD%D0%B0+%D1%83%D0%BC%D0%B5%D0%BD%D1%8C%D1%88%D0%B5%D0%BD%D0%B8%D0%B5+%D1%8D%D0%BD%D1%82%D1%80%D0%BE%D0%BF%D0%B8%D0%B8.+%D0%98+%D0%BB%D0%BE%D0%BB%D0%B5%D0%B9&target-visibility=on&project-name=Erlach+IBS&project-site=https%3A%2F%2Ferlach.ru&button-text=02&comment=on&hint=%D0%94%D0%BE%D0%B1%D0%B0%D0%B2%D1%8C%D1%82%D0%B5+%D0%BA%D0%BE%D0%BC%D0%BC%D0%B5%D0%BD%D1%82%D0%B0%D1%80%D0%B8%D0%B9%2C+%D0%BB%D0%B8%D0%B1%D0%BE+%D0%BE%D1%81%D1%82%D0%B0%D0%B2%D1%8C%D1%82%D0%B5+%D0%BF%D0%BE%D0%BB%D0%B5+%D0%BF%D1%83%D1%81%D1%82%D1%8B%D0%BC&successURL=https%3A%2F%2Ferlach.ru' width='488' height='201'></iframe>">>.