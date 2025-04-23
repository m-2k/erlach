-module(avz).
-author('Maxim Sokhatsky').
-compile(export_all).
-include_lib("avz/include/avz.hrl").
-include_lib("n2o/include/wf.hrl").
-include_lib("kvs/include/user.hrl").

-include_lib("db/include/user.hrl").
-include_lib("db/include/token.hrl").

callbacks(Methods) -> [ M:callback() || M <- Methods].
sdk(Methods) -> [ M:sdk() || M <- Methods].
buttons(Methods) -> [ M:login_button() || M <- Methods].

event(init) -> [];
event(logout) -> wf:user(undefined), wf:redirect(?LOGIN_PAGE);
event(to_login) -> wf:redirect(?LOGIN_PAGE);
event({Method,Event}) -> Method:event({Method,Event});
event(Ev) ->  wf:info(?MODULE,"Page Event ~p",[Ev]).

api_event(plusLogin, Args, Term) -> google:api_event(plusLogin, Args, Term);
api_event(fbLogin, Args, Term)   -> facebook:api_event(fbLogin, Args, Term);
api_event(winLogin, Args, Term)  -> microsoft:api_event(winLogin, Args, Term);
api_event(Name, Args, Term)      -> wf:error(?MODULE,"Unknown API event: ~p ~p ~p",[Name, Args, Term]).

login_user(User) -> wf:user(User), wf:redirect(?AFTER_LOGIN).
login(_Key, [{error, E}|_Rest])-> wf:error(?MODULE,"OAuth error: ~p", [E]);
login(Key, Args) ->
    wf:info(?MODULE,"Login: ~p, Key:~p, Args:~p",[?CTX#cx.module,Key,Args]),
	case kvs:get(token,{Key,Key:token_prop(Args,Key)}) of
        {ok,#token{user=UserID}=Token} ->
			wf:info(?MODULE,"Token exist in KVS: ~p",[Token]),
            (?CTX#cx.module):event({login, twitter, Token, Args});
        {error,_} ->
            (?CTX#cx.module):event({register, twitter, Args});
        U -> wf:info(?MODULE, "Login unknown: ~p",[U]) end.
