-module(eauth_mail).
-compile(export_all).
-author('Andy').
-include("eauth.hrl").
-include("eauth_user.hrl").

send_mail(Email,MessageFun) ->
    Password=eauth_utils:password(),
    Salt=eauth_utils:salt(),
    PasswordHash=eauth_utils:password_hash(Password,Salt),
    Login=wf:config(eauth,smtp_login),
    Passwd=wf:config(eauth,smtp_password),
    Relay=wf:to_list(wf:config(eauth,smtp_relay)),
    TLS=wf:config(eauth,smtp_tls,?SMTP_TLS),
    
    {Subject,Body}=MessageFun(Password),
    Mail={<<"text">>,<<"html">>,[{<<"From">>,Login},{<<"To">>,Email},
        {<<"Subject">>,list_to_binary([Subject])}],[],list_to_binary([Body])},
        
    gen_smtp_client:send({Login, [Email], mimemail:encode(Mail)},
        [{relay, Relay},{tls, TLS},{username, Login},{password, Passwd}]),
    
    {PasswordHash,Salt}.
