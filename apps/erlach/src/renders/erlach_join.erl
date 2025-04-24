-module(erlach_join).
-author('Andy').
-compile(export_all).

-include("erlach.hrl").
-include_lib("eauth/include/eauth_user.hrl").

title(#st{}) -> <<"Join – Erlach"/utf8>>.
urn() -> <<"join">>.

is_allow_join() -> {wf:config(erlach,auth,false),wf:config(erlach,join,false)} =:= {true,true}.

init(#route{level=?UNDEF}=Route) ->
    wf:info(?M,"init ~p",[self()]),
    case is_allow_join() of
        false -> {redirect,erlach_qs:mp(main)};
        true ->
            case eauth_user:is_alive(eauth_user:get()) of
                true -> {redirect,erlach_qs:mp(main)};
                false -> {ok,#st{user=?UNDEF,route=Route,action=view}}
            end
    end;
init(#route{level=Key}=_R) -> % joining from email link
    wf:info(?M,"init: join +KEY",[]),
    case is_allow_join() of
        false -> skip;
        true ->
            case eauth_user:is_alive(eauth_user:get()) of
                true -> skip;
                false ->
                    case eauth:activate(Key) of
                        {error,activation_not_found} -> skip;
                        {ok,{Mail,PasswordHash,Salt}} ->
                            Uid=kvs:next_id(user,1),
                            User=#user{id=Uid,email=Mail,password=PasswordHash,salt=Salt},
                            {ok,#user{}}=erlach_feeds:append(User),
                            wf:user(Uid),
                            wf:info(?M,"New user created ~p",[Uid])
                    end
            end
    end,
    {redirect,erlach_qs:mp(main)}.
finalize(#st{}) -> wf:info(?M,"finalize ~p",[self()]).
terminate() -> wf:info(?M,"terminate ~p",[self()]).

    
keyboardInputHelper() -> "keyboardInputHelper('[id=email] + button');".

render(content=Panel,#st{route=#route{option={status,Status}}}) ->
    Body=[
        #panel{id=auth,body=#panel{class=[fl,vert],body=[
        render_result(Status)
    ]}}],
    render_content(Panel,Body);
render(content=Panel,#st{}=S) ->
    render_content(Panel,render_join()).


render_content(Panel,Body) ->
    #panel{id=Panel,body=[
        #panel{class= <<"content-title">>,body=[
            #span{class=section,body=?TR(<<"Регистрация"/utf8>>,<<"Register">>)},
            ?TR(<<"в Эрлаче"/utf8>>,<<"in Erlach">>)
        ]},
        Body,
        #span{class= <<"remark orange">>,body=?TR(<<"Есть аккаунт? Тебе сюда"/utf8>>,
            <<"Already have an account? Сome here">>)},
        #panel{class=[fl,cent],body=#a{class=[b,xl,orange],
            body=?TR(<<"Вход в Эрлач по паролю"/utf8>>,<<"Sign In to Erlach with password">>),
            postback=erlach_qs:mp(signin),href=erlach_qs:ml(signin)}}        
    ]}.
render_join() ->
    [ #span{class=[remark,blue],body=?TR(<<"Просто позволь нам выслать тебе письмо"/utf8>>,
        <<"Simply allow us to send U a letter">>)},
    #panel{id=auth, actions=keyboardInputHelper(), body=[
        #panel{class= <<"fl login-form">>,body=[
            #textbox{id=email,class=xl,placeholder="Email",autofocus= <<"autofocus">>},
            #button{class=xl,
                body=?TR(<<"Отправить мне инвайт"/utf8>>,<<"Send me invite">>),
                onclick="this.disabled = true",
                postback=#render_event{render=?M,target=auth,event=join},source=[email]}
        ]}
    ]}].
render_result({ok,Mail}) ->
    [ #h2{class= <<"post-topic">>,body=?TR(<<"Поздравляем!"/utf8>>,<<"Congratulations!">>)},
    #span{class=legend,body=?TR(<<"Ты почти зарегистрирован"/utf8>>,<<"You are almost registered">>)},
    #span{class=legend,body=?TR(<<"В твой почтовый ящик было отправлено письмо, сходи туда и жмакай в письме ссылку для активации аккаунта"/utf8>>,
        <<"On Ur mailbox has been sent a letter, go to it via the link to activate your account to complete registration">>)} ];
render_result({error,{network_error,_}}) ->
    [ #h2{class= <<"post-topic">>,body=?TR(<<"Что-то пошло не так"/utf8>>,<<"Something went wrong">>)},
    #span{class=legend,body=?TR(<<"У нас не получилось зарегистрировать тебя на Эрлаче. Пожалуйста, расскажите нам об этом инциденте"/utf8>>,
        <<"We could not register you to our website. Please report it to customer support">>)} ];
render_result({error,{already_registered,_}}) ->
    [ #h2{class= <<"post-topic">>,body=?TR(<<"Ну что же ты"/utf8>>,<<"Failure">>)},
    #span{class=legend,body=?TR(<<"Этот почтовый ящик уже зарегистрирован"/utf8>>,<<"This mailbox is already registered">>)} ];
render_result({error,{wait_activation,_}}) ->
    [ #h2{class= <<"post-topic">>,body=?TR(<<"Всё не так уж плохо"/utf8>>,<<"Things are not so bad">>)},
    #span{class=legend,body=?TR(<<"Мы ожидаем подтверждения регистрации"/utf8>>,<<"We expect registration confirmation">>)},
    #span{class=legend,body=?TR(<<"Для этого вам нужно найти письмо в почтовом ящике, которое мы отправили и завершить регистрацию, нажав на ссылку в письме. Не так уж сложно, правда?"/utf8>>,
        <<"For this you need to find a letter in the mailbox to send us and activate your account by clicking on the link in that email. It is not so difficult">>)} ];
render_result({error,{wrong_email,_}}) ->
    [ #h2{class= <<"post-topic">>,body=?TR(<<"Ваша почта нам не подходит"/utf8>>,<<"Bad e-mail">>)},
    #span{class=legend,body=?TR(<<"По некоторым причинам мы не регистрируем аккаунты для некоторых почтовых служб. Попробуйте зарегистрировать с помощью популярных сервисов электронной почты"/utf8>>,
        <<"For some reason we limit unwanted mailboxes. Try to register using the popular e-mail services">>)} ].


event(#render_event{target=auth,event=join}) ->
    Mail=wf:q(email),
    ActivateMessageFun=fun(Password,Key) ->
        ActivateUrl=[wf:config(erlach,domain,<<"https://erlach.co">>),"/",urn(),"/",Key],
        Subject="Welcome to Erlach!",
        Body=["<h1>Thanks you for registering on Gramach Imageboard Services</h1>",
            "<p>You login: ",wf:html_encode(Mail),"</p>",
            "<p>You password: ",wf:html_encode(Password),"</p>",
            "<p>Click this link to finishing registration: <a href=\"",ActivateUrl,"\">Activate account link</a></p>"],
        {Subject,Body}
    end,
    
    Status=eauth:join(Mail,ActivateMessageFun),
    spa:redirect((erlach_qs:mp(join))#postback{history=false,route_option={status,Status}});
event(Unknown) -> ?EVENT_ROUTER:unknown(?M,Unknown).