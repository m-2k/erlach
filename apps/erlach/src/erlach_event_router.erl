-module(erlach_event_router).
-compile(export_all).
-author('andy').

-include_lib("n2o/include/wf.hrl").
-include("erlach.hrl").

%%% Preprocessing
proxy({server,#postback{}=E}) -> proxy(E);
proxy({server,#pubsub{}=E}) -> proxy(E);

%%% Global Events
proxy(#postback{}=P) -> ?SPA:navigate(P);
proxy({client,{attachment,_,_,_,_}=E}) -> render_call(E,?UNDEF);

%%% N2O Events
proxy(#ftp{}=E) -> render_call(E,?UNDEF);
proxy({server,_}=E) -> render_call(E,?UNDEF);

%%% Render Events
proxy(#render_event{guard=G}=E) -> render_call(E,G);
proxy(#pubsub{}=E) -> render_call(E,?UNDEF);
proxy(#view{}=E) -> render_call(E,?UNDEF);
proxy(#create{}=E) -> render_call(E,?UNDEF);
proxy(#edit{}=E) -> render_call(E,?UNDEF);
proxy(#delete{}=E) -> render_call(E,?UNDEF);
proxy(#cancel{}=E) -> render_call(E,?UNDEF);
proxy(#update{}=E) -> render_call(E,?UNDEF);
proxy(#add{}=E) -> render_call(E,?UNDEF);
proxy(#put{}=E) -> render_call(E,?UNDEF);
proxy(#subscribe{}=E) -> render_call(E,?UNDEF);
proxy(#unsubscribe{}=E) -> render_call(E,?UNDEF);
proxy(E) -> unknown(case target() of unknown -> ?M; {module,_} -> ?M; {render,R} -> R end, E).

%%% Allowed Renders
enabled(erlach_main) -> true;
enabled(erlach_board) -> true;
enabled(erlach_thread) -> true;
enabled(_) -> false.

render_call(E,Guard) ->
    case target() of
        unknown -> unknown(?M,E);
        {module,M} -> not_allowed(M,E);
        {render,Render} -> case enabled(Render) of
            true -> ok=guard(Guard,erlang:get(state)), Render:event(E);
            _ -> not_allowed(Render,E) end end.

target() -> case erlang:get(state) of
    #st{route=#route{render=?UNDEF,module=M}} -> {module,M};
    #st{route=#route{render=R}} -> {render,R};
    _ -> unknown end.

%%% Unsafe funs with throws({guard,Reason})
guard(?UNDEF,#st{}) -> ok;
guard(is_user_authorized,#st{}) -> ok;
guard(is_role_moderate,#st{}) -> ok;
guard(_,_) -> throw({guard,not_defined_guard}).

guard_error(M,E,G) -> wf:warning(M,"Guard not passed: ~p from module: ~p for event: ~p", [G,M,E]).
unknown(M,E) -> wf:warning(M,"Unknown event: ~p from module: ~p", [E,M]).
not_allowed(M,E) -> wf:warning(M,"Not allowed event: ~p from module: ~p", [E,M]).
