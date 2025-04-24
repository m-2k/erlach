-module(eauth_event).
-compile(export_all).
-author('Andy').
-include("eauth.hrl").
-include("eauth_user.hrl").


    
signin(<<>>,_) -> wf:warning(?M,"Signin with empty email field",[]), eauth_utils:wait(), {error,login_required};
signin(_,<<>>) -> wf:warning(?M,"Signin with empty password field",[]), eauth_utils:wait(), {error,password_required};
signin(Mail,Passwd) ->
    case kvs:index(user,email,Mail) of
        [#user{id=Uid,password=PasswordHash,salt=Salt}=User] ->
            case eauth_utils:password_hash(Passwd,Salt) of
                PasswordHash ->
                    wf:user(Uid),
                    wf:info(?MODULE,"User: ~p (~p)",[Uid,Mail]),
                    {ok,User};
                _WrongPasswordHash ->
                    wf:warning(?M,"Signin failed of user ~p (~p)",[Uid,Mail]),
                    eauth_utils:wait(),
                    {error,wrong_password}
            end;
        NotFound ->
            wf:warning(?M,"User with mail ~p not found: ~p",[Mail,NotFound]),
            eauth_utils:wait(),
            {error,user_not_found}
    end.

reset(Mail,MessageFun) ->
    eauth_utils:wait(),
    case kvs:index(user,email,Mail) of
        [#user{}=User] ->
            case eauth_user:is_alive(User) of
                true ->
                    {PasswordHash,Salt}=eauth_mail:send_mail(Mail,MessageFun),
                    wf:info(?M,"Reset password done for user with mail ~p",[Mail]),
                    {ok,{PasswordHash,Salt}};
                false ->
                    wf:warning(?M,"Reset password failed, user with mail ~p deleted",[Mail]),
                    {error,user_deleted}
            end;
        NotFound ->
            wf:warning(?M,"Reset password failed, user with mail ~p not found: ~p",[Mail,NotFound]),
            {error,user_not_found}
    end.
reset(Mail,MessageFun,[activate]) ->
    eauth_utils:wait(),
    case wf:cache({tag(),Mail}) of
        ?UNDEF ->
            case kvs:index(user,email,Mail) of
                [_] ->
                    wf:info(?M,"Sending mail to: ~p",[Mail]),
                    try
                        Key=key(),
                        MessageFun2=fun(Password) -> MessageFun(Password,Key) end,
                        {PasswordHash,Salt}=eauth_mail:send_mail(Mail,MessageFun2),
                        Till=till(),
                        wf:cache({tag(),Key},{Mail,PasswordHash,Salt},Till),
                        wf:cache({tag(),Mail},Key,Till),
                        wf:info(?M,"Sending reset mail to: ~p [OK]",[Mail]),
                        {ok,Mail}
                    catch
                        T:E ->
                            wf:error(?M,"Sending reset mail to: ~p FAIL, ~p:~p",[Mail,T,E]),
                            {error,{network_error,Mail}}
                    end;
                [] ->
                    wf:warning(?M,"Reset error: user not joined ~p",[Mail]),
                    {error,{already_registered,Mail}}
            end;
        Activation ->
            wf:warning(?M,"Resetting. Wait activation for: ~p",[Mail]),
            {error,{wait_activation,Mail}}
    end.

tag() -> wf:config(eauth,cache_activation_tag,?CACHE_ACTIVATION_TAG).
key_length() -> wf:config(eauth,activation_key_length,?ACTIVATION_KEY_LENGTH).
key() -> wf:to_binary(wf:hex_encode(crypto:strong_rand_bytes(key_length()))).
till() -> n2o_session:till(calendar:local_time(),wf:config(eauth,activation_time,?ACTIVATION_TIME)).

join(Mail,MessageFun) ->
    eauth_utils:wait(),
    case eauth_utils:email_validation(Mail) of
        {ok,Mail2} ->
            case wf:cache({tag(),Mail2}) of
                ?UNDEF ->
                    case kvs:index(user,email,Mail2) of
                        [] ->
                            wf:info(?M,"Sending mail to: ~p",[Mail2]),
                            try
                                Key=key(),
                                MessageFun2=fun(Password) -> MessageFun(Password,Key) end,
                                {PasswordHash,Salt}=eauth_mail:send_mail(Mail2,MessageFun2),
                                Till=till(),
                                wf:cache({tag(),Key},{Mail2,PasswordHash,Salt},Till),
                                wf:cache({tag(),Mail2},Key,Till),
                                wf:info(?M,"Sending mail to: ~p [OK]",[Mail2]),
                                {ok,Mail2}
                            catch
                                T:E ->
                                    wf:error(?M,"Sending mail to: ~p FAIL, ~p:~p",[Mail2,T,E]),
                                    {error,{network_error,Mail2}}
                            end;
                        _ ->
                            wf:warning(?M,"User already joined ~p",[Mail2]),
                            {error,{already_registered,Mail2}}
                    end;
                Activation ->
                    wf:warning(?M,"Wait activation for: ~p",[Mail2]),
                    {error,{wait_activation,Mail2}}
            end;
        {error,Mail2} ->
            wf:warning(?M,"Wrong mail for register: ~p",[Mail2]),
            {error,{wrong_email,Mail2}}
    end.

activate(Key) ->
    eauth_utils:wait(),
    case wf:cache({tag(),Key}) of
        ?UNDEF -> {error,activation_not_found};
        {Mail,PasswordHash,Salt}=Cache ->
            wf:cache({tag(),Key},?UNDEF), % clear
            wf:cache({tag(),Mail},?UNDEF), % clear
            {ok,Cache}
    end.
