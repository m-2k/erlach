-module(profile). %% MAIN PAGE / BOARDS LISTING
-author('andy').
-vsn('0.0.0').

-compile(export_all).
-include_lib("n2o/include/wf.hrl").
-include_lib("kvs/include/kvs.hrl").
-include_lib("kvs/include/feed.hrl").
-include_lib("db/include/post.hrl").
-include_lib("db/include/token.hrl").
-include_lib("db/include/attachment.hrl").

-include_lib("db/include/board.hrl").
-include_lib("db/include/user.hrl").
-include("erlach.hrl").

-ifndef(SESSION).
-define(SESSION, (wf:config(n2o,session,erlach_session))).
-endif.

peer()    -> io_lib:format("~p",[wf:peer(?REQ)]).
main()    ->
    _R = avz:callbacks([twitter]),
    #dtl{file="erlach",app=erlach,bindings=[{body,body()}, {title,<<"Profile">>}]}.

body() ->
    Content = case ?SESSION:erase_param(?MODULE) of
        signin -> #panel{class= <<"center">>,body=#link{body= <<"Auth with Twitter">>,
            class= <<"button success">>, postback={twitter,logintwitter}} };
        _ ->
            User = u:get(),
            Title = case u:is_temp(User) of
                true -> <<"Anonymous">>;
                false -> guard:html_escape(User#user3.name)
            end,
            [ #panel{class= <<"content-title">>,body=Title },
                html:username_form(),
                #panel{body=[
                ]} ]
    end,
    html:body([ case config:debug() of true -> #span{body= wf:f("User: ~p", [u:get()])}; _ -> [] end, Content]).

acc_list() -> [].

event(init) -> ok;
event({name,write}=Action) ->
    UserName = wf:to_binary(wf:q(username)),
    User = u:get(),
    case {re:run(UserName, "^(?=.{6,26}$)(?![_.])(?!.*[_.]{2})[a-z0-9._]+(?<![_.])$"), u:is_temp(User) } of
        {{match, _}, false} when UserName =/= <<"anonymous">> ->
            case kvs:get(name,UserName) of
                {ok, _Exist} -> html:warning("Already exist, sorry, bro");
                {error, not_found} ->
                    Name=#name{
                        id=UserName,
                        feed_id={name,u:id(User)},
                        created=erlang:now(),
                        banned=false,
                        deleted=false,
                        birthday= <<>>
                        },
                    {ok, _Name2}=kvs:add(Name),
                    
                    wf:update(<<"username-manage">>, html:username_form(Action)),
                    html:success("Yay! U now known also as " ++ wf:to_list(UserName));
                E -> wf:error(?MODULE, "Unable to lookup name: ~p into DB: ~p",[UserName,E]),
                    html:error("Some error, plz refresh page and try again")
            end;
        _ -> html:warning("Wrong name, allowed 6-26 lowercase symbols: 'a-z', '.' and '_'")
    end, ok;
event({name,modify,Name}=Action) ->
    case {kvs:get(name,Name), u:id()} of
        {{ok, #name{feed_id={name,Uid}}=N}, Uid} ->
            wf:update(<<"username-manage">>, html:username_form({name,modify,N}));
        _ -> skip
    end;
event({name,update,Name}=Action) ->
    case {kvs:get(name,Name), u:id()} of
        {{ok, #name{feed_id={name,Uid}}=N}, Uid} ->
            kvs:put(N#name{birthday=wf:q(about)}),
            wf:update(<<"username-manage">>, html:username_form(Action));
        _ -> skip
    end;
event({name, _}=Action) ->
    wf:update(<<"username-manage">>, html:username_form(Action));
event(terminate) -> wf:info(?MODULE,"Terminate",[]);

% exist
event({login, twitter, #token{user=UserID}, _Args}) ->
    wf:info(?MODULE, "Login Event, user: ~p", [u:get()]),
    case kvs:get(user3,UserID) of
        {ok, User} ->
            u:put(User),
            wf:redirect("/profile")
    end;
    % avz:login_user(U);

% new
event({register, twitter=Key, Args}) ->
    User = u:ensure_user(),
    TokenID = Key:token_prop(Args, Key),
    Name = proplists:get_value(<<"screen_name">>, Args),
    u:put(User#user3{tokens=[{Key,TokenID}|User#user3.tokens],name=Name}),
    Stored = u:store_temp_user(),
    Token = #token{id={Key,TokenID}, user=Stored#user3.id},
    kvs:put(Token), % #token does't have chain
    wf:info(?MODULE, "Register Event, new tokens: ~p", [Stored#user3.tokens]),
    wf:redirect("/profile");
    % avz:login_user(U);

event(Event) -> guard:shared_event(?MODULE, Event).
% api_event(X,Y,Z) -> avz:api_event(X,Y,Z).