-module(erlach_event_router).
-compile(export_all).
-author('Andy').

-include("erlach.hrl").

%%% Preprocessing
proxy({server,#postback{}=E}) -> proxy(E);
proxy({server,#pubsub{}=E}) -> proxy(E);
proxy({server,#view{partial=true}=E}) -> proxy(E);

%%% Global Events
proxy(#auth{}=A) -> erlach_auth:auth(A);
proxy(#postback{}=P) -> ?SPA:navigate(P);
proxy(#pubsub{target=subscription}=Pub) -> render_call(Pub,erlach_subscription);
proxy({client,{attachment,_,_,_,_}=E}) -> render_call(E);

%%% N2O Events
proxy(#ftp{}=E) -> render_call(E);
proxy({server,_}=E) -> render_call(E);

%%% Render Events
proxy(#render_event{mod=M}=E) -> render_call(E,M);
proxy(#pubsub{mod=M}=E) -> render_call(E,M);
proxy(#view{mod=M}=E) -> render_call(E,M);
proxy(#create{mod=M}=E) -> render_call(E,M);
proxy(#edit{mod=M}=E) -> render_call(E,M);
proxy(#delete{mod=M}=E) -> render_call(E,M);
proxy(#cancel{mod=M}=E) -> render_call(E,M);
proxy(#update{mod=M}=E) -> render_call(E,M);
proxy(#add{mod=M}=E) -> render_call(E,M);
proxy(#put{mod=M}=E) -> render_call(E,M);
proxy(E) -> unknown(ensure_target(), E).

%%% Allowed Renders
enabled(erlach_spa) -> true;
enabled(erlach_main) -> true;
enabled(erlach_board) -> true;
enabled(erlach_thread) -> true;
enabled(erlach_subscription) -> true;
enabled(erlach_about) -> true;
enabled(_) -> false.

access(#delete{}) -> erlach_auth:access() =:= full;
access(#edit{}) -> erlach_auth:access() =:= full;
access(_) -> true.

ensure_target() -> ensure_target(?UNDEF).
ensure_target(Mod) -> case target(Mod) of unknown -> Mod; {_,T} -> T end.

target(?UNDEF) -> target();
target(Mod) -> {custom,Mod}.
target() -> case spa:st() of
    #st{route=#route{render=?UNDEF,module=M}} -> {module,M};
    #st{route=#route{render=R}} -> {render,R};
    _ -> unknown end.

render_call(E) -> render_call(E,?UNDEF).
render_call(E,Mod) ->
    case access(E) of
        true ->
            case target(Mod) of
                unknown -> unknown(?M,E);
                {module,M} -> not_allowed(M,E);
                {render,R} -> case enabled(R) of true -> R:event(E); _ -> not_allowed(R,E) end;
                {custom,C} -> case enabled(C) of true -> C:event(E); _ -> not_allowed(C,E) end
            end;
        false -> no_access(ensure_target(Mod),E)
    end.

unknown(M,E) -> wf:warning(?M,"Unknown event: ~p for module: ~p", [format(E),M]).
not_allowed(M,E) -> wf:warning(?M,"Not allowed event: ~p for module: ~p", [format(E),M]).
no_access(M,E) -> wf:warning(?M,"No access to execute event: ~p for module: ~p", [format(E),M]).


%%% Prevent MEM-leaking for huge data in events
% format(#view{queue=Q}=E) -> E#view{queue={'count:',length(Q)}};
format(E) when is_tuple(E) -> element(1,E);
format(E) -> E.
