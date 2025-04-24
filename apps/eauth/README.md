# EAUTH
**Authorization library for Synrc Erlang Stack**

## Overview

* Simple e-mail authorization library for your web-application on [n2o](https://github.com/5HT/n2o) / [kvs](https://github.com/synrc/kvs).

## Dependency

* WebSocket Application Server for Enterprises [n2o](https://github.com/5HT/n2o)
* Abstract Term Database [kvs](https://github.com/synrc/kvs)
* SMTP server and client [gen_smtp](https://github.com/Vagabond/gen_smtp)
* Unicode eXtention [ux](https://github.com/erlang-one/ux)

## Installation

Open you project and modify dependency section in file `rebar.config`:

```
{deps, [
    
    ***
    
    {eauth,       ".*", {git, "git://github.com/erlang-one/eauth",      {tag, "master"} }}
]}.
```

Open you project and setup basic settings in file `sys.config`:
```
[
    ***
    
    {eauth, [
        {hash_algorithm,sha512},
        {salt_local,<<63,0,56,7>>},
        {wrong_delay,400},
        {password_symbols,<<"abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789">>},
        {password_length,16},
        {salt_length,4},
        {smtp_login,<<"mailbox@domain.com">>},
        {smtp_password,<<"application_password">>},
        {smtp_relay,<<"smtp.mailservice.com">>},
        {smtp_tls,always}
    ]}
]
```

All config fields you can find in `eauth/include/eauth.hrl` file, just translate names to lowercase.

## Api

#### Sign In

* `eauth:signin(Mail :: binary(), Passwd :: binary()) -> {error, Error} | {ok, User}.`
* `Error :: login_required | password_required | wrong_password | user_not_found`
* `User :: #user{}`, see `eauth/include/eauth_user.hrl`
* Snippet:

```erlang
    case eauth:signin(wf:q(login),wf:q(passwd)) of
        {ok,#user{id=Uid}=User} -> wf:user(Uid);
        {error,Error} -> wf:warning(?MODULE,"Error signin: ~p",[Error])
    end
```

#### Activate

* `eauth:activate(Key :: binary) -> {error,activation_not_found} | {ok,{Mail,PasswordHash,Salt}}.`
* `Mail2 :: binary()`
* `PasswordHash :: binary()`
* `Salt :: binary()`
* Snippet:

```erlang
    case eauth:activate(Key) of
        {error,activation_not_found} -> skip;
        {ok,{Mail,PasswordHash,Salt}} ->
            Uid=kvs:next_id(user,1),
            {ok,_}=kvs:add(#user{id=Uid,email=Mail,password=PasswordHash,salt=Salt}),
            wf:user(Uid),
            wf:info(?MODULE,"New user created ~p",[Uid])
    end
```

#### Join

* `eauth:join(Mail,JoinMessageFun) -> {error, {Error, Mail2}} | {ok, Mail2)}.`
* `Error :: network_error | already_registered | wait_activation | wrong_email`
* `Mail2 :: binary()`
* Snippet:

```erlang
    Mail=wf:q(email),
    Status=eauth:join(Mail,fun(Password,Key) ->
        ActivateUrl=["http://domain.com/join/",Key],
        Subject="Welcome!",
        Body=["<h1>Thanks you for registering</h1><p>You password: ",wf:html_encode(Password),"</p>",
            "<p>Finishing registration link: <a href=\"",ActivateUrl,"\">",wf:html_encode(ActivateUrl),"</a></p>"],
        {Subject,Body}.)
```

#### Reset password
* `eauth:reset(Mail,resetMessage) -> {error, Error} | {ok,{PasswordHash, Salt}}`
* `Error :: user_deleted | user_not_found`
* `PasswordHash :: binary()`
* `Salt :: binary()`
* Snippet:

```erlang
    Mail=wf:q(login),
    ResetMessageFun=fun(Password) ->
        Subject="Reset password",
        Body=["<h1>You password has been resetting</h1><p>You password: ",wf:html_encode(Password),"</p>"],
        {Subject,Body}
    end,
    case eauth:reset(Mail,ResetMessageFun) of
        {error,_} -> ok;
        {ok,{PasswordHash,Salt}} ->
            case kvs:index(user,email,Mail) of
                [#user{}=User] -> kvs:put(User#user{password=PasswordHash,salt=Salt});
                _ -> skip
            end
    end
```

## User / State access

Use `-include_lib("eauth/include/eauth_user.hrl").` in you project for define `#user{}`.

* `eauth_user:get() -> #user{} | undefined` – Get session user from database
* `eauth_user:user() -> #user{} | undefined` – Get user from state
* `eauth_user:user(User) -> User` – Write user to state
* `eauth_user:is_alive(#user{} | any()) -> true | false` – Check for non-deleted user

By default, user (in state) stored in process state.
You can override this if want store user in custom state.
Just setup `function_state` and `function_state_user` in `sys.config` as `{Mod,Fun}`.
```erlang
function_state() -> State % get custom state
function_state(State) -> State % write custom state
function_state_user(State) -> User % get from custom state
function_state_user(State,User) -> State % write to custom state
```

## Credits

* [@m-2k](https://github.com/m-2k)
* [@rusjava8](https://github.com/rusjava8)
