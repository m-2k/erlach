-module(erlach_signin).
-author('Andy').
-compile(export_all).

-include("erlach.hrl").
-include_lib("eauth/include/eauth_user.hrl").


title(#st{}) -> <<"Sign in – Erlach"/utf8>>.
urn() -> <<"signin">>.

is_allow_signin() -> true =:= wf:config(erlach,auth,false).

init(#route{}=Route) ->
    wf:info(?M,"init ~p",[self()]),
    
    case is_allow_signin() of
        false -> {redirect,erlach_qs:mp(main)};
        true ->
            case eauth_user:is_alive(eauth_user:get()) of
                true -> {redirect,erlach_qs:mp(main)};
                false -> {ok,#st{user=?UNDEF,route=Route,action=view}}
            end
    end.
finalize(#st{}) -> wf:info(?M,"finalize ~p",[self()]).
terminate() -> wf:info(?M,"terminate ~p",[self()]).


render(content=Panel,#st{route=#route{level= <<"reset-password">>,option={status,{ok,_Mail}}}}=S) ->
    render_content(Panel,render_reset_ok());
render(content=Panel,#st{route=#route{level= <<"reset-password">>}}=S) ->
    render_content(Panel,render_reset());
render(content=Panel,#st{}=S) ->
    render_content(Panel,render_auth()).
    
render_content(Panel,Body) ->
    #panel{id=Panel,body=[
        #panel{class= <<"content-title">>,body=[
            #span{class=section,body=?TR(<<"Войти"/utf8>>,<<"Sign in">>,<<"Зайти"/utf8>>)},
            ?TR(<<"в Эрлач"/utf8>>,<<"to Erlach">>,<<"на Эрлач"/utf8>>)
        ]},
        Body,
        #span{class= <<"remark orange">>,body=?TR(<<"Создай учётную запись за 15 секунд, если ещё не сделал этого"/utf8>>,
            <<"Create your account in 15 seconds, if you have not done so">>,
            <<"Створи обліковий запис за 15 секунд, якщо ще цього не зробив"/utf8>>)},
        #panel{class=[fl,cent],body=#a{class=[b,xl,orange],
            body=?TR(<<"Создать аккаунт в Эрлаче"/utf8>>,<<"Create Erlach Account">>,<<"Створити акаунт на Эрлачі"/utf8>>),
            postback=erlach_qs:mp(join),href=erlach_qs:ml(join)}}
        
    ]}. 
render_auth() ->
    [ #span{class= <<"remark blue">>,body=?TR(<<"Авторизовывайся если уже имеешь аккаунт"/utf8>>,
        <<"Sign in if you already have an account">>,<<"Авторизуйся, якщо вже маєш аккаунт"/utf8>>)},
    #panel{id=auth, actions=keyboardInputHelper(multiple), body=[
        #panel{class= <<"fl login-form">>,body=[
            #textbox{id=login,class=xl,placeholder= <<"Email">>,autofocus= <<"autofocus">>},
            #password{id=passwd,class=xl,placeholder= <<"Password">>},
            #button{class=xl,
                body=?TR(<<"Войти"/utf8>>,<<"Sign in">>,<<"Зайти"/utf8>>),
                onclick="this.disabled = true",
                postback=#render_event{target=auth,event=signin},
                source=[login,passwd]},
            #panel{class=addition,body=#a{body=?TR(<<"Сбросить пароль"/utf8>>,<<"Reset password">>,<<"Скинути пароль"/utf8>>),
                postback=erlach_qs:mp({signin,<<"reset-password">>})}}
        ]}
    ]}].
render_reset() ->
    [ #span{class=[remark,sea],body=?TR(<<"Мы вышлем тебе новый пароль"/utf8>>,
        <<"We will send U a new password">>,
        <<"Ми вишлемо тобі новий пароль"/utf8>>)},
    #panel{id=auth, actions=keyboardInputHelper(single), body=[
        #panel{class= <<"fl login-form">>,body=[
            #textbox{id=login,class=[xl,sea],placeholder= <<"Email">>,autofocus= <<"autofocus">>},
            #button{class=[xl,sea],
                body=?TR(<<"Сбросить пароль"/utf8>>,<<"Reset password">>,<<"Скинути пароль"/utf8>>),
                onclick="this.disabled = true",
                postback=#render_event{target=auth,event=reset_activate},source=[login]},
            #panel{class=addition,body=#a{class=[signin,xl,sea],body=?TR(<<"Войти по паролю"/utf8>>,
                <<"Sign in with password">>,<<"Зайти з паролем"/utf8>>),
                postback=erlach_qs:mp(signin)}}
        ]}
    ]}].
render_reset_ok() ->
    [ #span{class= <<"remark blue">>,body=?TR(<<"Проверяй почту!"/utf8>>,
        <<"Check U mailbox!">>,<<"Перевір поштову скриньку!"/utf8>>)},
    #panel{id=auth,body=#panel{class= <<"fl vert">>,body=[
        #h2{class= <<"post-topic">>,body=?TR(<<"Наши поздравления!"/utf8>>,<<"Congratulations!">>,<<"Наші вітання!"/utf8>>)},
        #span{class=legend,body=?TR(<<"Пароль был сгенерирован заново и выслан на почту"/utf8>>,
            <<"Password has been updated and sent to your mailbox">>,
            <<"Пароль був згенерований заново та висланий на пошту"/utf8>>)},
        #a{class=[xl,sea],body=?TR(<<"Войти по паролю"/utf8>>,<<"Sign in with password">>,<<"Зайти з паролем"/utf8>>),
            postback=erlach_qs:mp(signin)}
    ]}}].
    
warning(Ru,En) ->
    erlach_markup:message(warn, [#span{class=ru,body=Ru},#span{class=en,body=En}], 4000),
    erlach_markup:highlight(warn).
keyboardInputHelper(multiple) -> "keyboardInputHelper('[id=login] + [id=passwd] + button');";
keyboardInputHelper(single) -> "keyboardInputHelper('[id=login] + button');".
    
event(#render_event{target=auth,event=signin}) ->
    case eauth:signin(wf:q(login),wf:q(passwd)) of
        {error,login_required} -> warning(<<"И зачем на меня жать?…"/utf8>>,<<"And what U need?…"/utf8>>);
        {error,password_required} -> warning(<<"Пароль-то введи"/utf8>>,<<"LOL, password required"/utf8>>);
        {error,wrong_password} -> warning(<<"Ты неправ"/utf8>>,<<"Wrong user/password"/utf8>>);
        {error,user_not_found} -> warning(<<"Ты неправ"/utf8>>,<<"Wrong user/password"/utf8>>);
        {ok,#user{id=Uid}=User} ->
            wf:user(Uid),
            erlach_markup:highlight(succ),
            case spa:st() of
                #st{route=#route{option={'back-postback',BackPostback}}} -> spa:redirect(BackPostback);
                _ -> spa:redirect(erlach_qs:mp(main))
            end;
        Unknown ->
            wf:warning(?M,"Unknown signin error: ~p",[Unknown])
    end,
    wf:wire(#jq{target={qs,"#auth button"},property="disabled=false"});
event(#render_event{target=auth,event=reset}) ->
    case wf:q(login) of
        <<>> -> warning(<<"Надо же мыло ввести"/utf8>>,<<"Mail required">>);
        Mail ->
            ResetMessageFun=fun(Password) ->
                SignInUrl=[wf:config(erlach,domain,<<"https://erlach.co">>),"/",urn()],
                Subject="Reset password for you account",
                Body=["<h1>You password has been resetting on Erlach Imageboard Services</h1>",
                    "<p>You login: ",wf:html_encode(Mail),"</p>",
                    "<p>You password: ",wf:html_encode(Password),"</p>",
                    "<p>Click this link to login: <a href=\"",SignInUrl,"\">Sign-in to Erlach</a></p>"],
                {Subject,Body}
            end,

            case eauth:reset(Mail,ResetMessageFun) of
                {error,_} -> ok;
                {ok,{PasswordHash,Salt}} ->
                    case kvs:index(user,email,Mail) of
                        [#user{}=User] ->
                            erlach_feeds:update(User,fun(User2) -> {ok,User2#user{password=PasswordHash,salt=Salt}} end);
                        _ -> skip
                    end
            end,
            erlach_markup:highlight(succ),
            spa:redirect((erlach_qs:mp({signin,<<"reset-password">>}))#postback{history=false,route_option={status,{ok,Mail}}})
    end;
event(#render_event{target=auth,event=reset_activate}) ->
    case wf:q(login) of
        <<>> -> warning(<<"Надо же мыло ввести"/utf8>>,<<"Mail required">>);
        Mail ->
            ResetMessageFun=fun(Password,Key) ->
                ActivateUrl=[wf:config(erlach,domain,<<"https://erlach.co">>),"/",urn(),"/",Key],
                Subject="Reset password for you account",
                Body=["<h1>You password has been resetting on Erlach Imageboard Services</h1>",
                    "<p>You login: ",wf:html_encode(Mail),"</p>",
                    "<p>You NEW password: ",wf:html_encode(Password),"</p>",
                    "<p>Click this link to activate account with new credentials: <a href=\"",ActivateUrl,"\">Activate account in Erlach</a></p>"],
                {Subject,Body}
            end,

            case eauth:reset(Mail,ResetMessageFun,[activate]) of
                {error,_} -> skip;
                {ok,Mail} -> erlach_markup:highlight(succ)
            end,
            spa:redirect((erlach_qs:mp({signin,<<"reset-password">>}))#postback{history=false,route_option={status,{ok,Mail}}})
    end;
event(#render_event{target=auth,event=logout}) ->
    wf:logout(),
    spa:redirect(erlach_qs:mp(main));
event(Unknown) -> ?EVENT_ROUTER:unknown(?M,Unknown).