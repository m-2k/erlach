-module(profile). %% MAIN PAGE / BOARDS LISTING
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
main()    -> % wf:redirect({http,<<"/sasay">>}).
    R = avz:callbacks([twitter]),
    #dtl{file="erlach",app=erlach,bindings=[{body,body()}, {title,<<"Profile">>}]}.
    
names_list() ->
    #panel{class= <<"flex-vertical">>, body = [
        [#button{class= <<"primary">>, body= <<"Add"/utf8>>, postback={showform, addname}}] ++ lists:map(fun(N) ->
            #span{class= <<"square">>, body= <<"", (guard:html_escape(N#name.displayname))/binary>>}
            end, u:names()) ]}.

body() ->
    Content = case ?SESSION:erase_param(?MODULE) of
        signin -> #button{body= <<"Auth with Twitter">>, class = <<"success center">>, postback={twitter,logintwitter}};
        _ -> #panel{class = <<"flex-horizontal">>, body=[
            names_list(),
            html:username_edit()]}
    end,
    % wf:context((?CTX)#cx{req=wf:header(<<"Location">>,<<"/sasay">>,?REQ)}),
    
    % Req1 = wf:header(<<"Access-Control-Allow-Origin">>, Origin, NewCtx#cx.req),
    % wf:state(status,301),
    % wf:redirect({http,<<"/sasay">>}),
    html:body([ case config:debug() of true -> #span{body= wf:f("User: ~p", [u:get()])}; _ -> [] end, Content]).

event(init) -> ok;
event({showform, addname}) ->
    wf:update(<<"username-manage">>, html:username_add());
event({username,add}) ->
    html:info("SASAY LALKA reg" ++ wf:temp_id()),
    html:success("SASAY LALKA reg" ++ wf:temp_id()),
    html:warning("SASAY LALKA reg" ++ wf:temp_id()),
    html:error("SASAY LALKA reg" ++ wf:temp_id());
event(join) ->
    case u:join() of
        {ok, User} ->
            wf:info(?MODULE, "User joined: ~p", [User]),
            wf:update(joinpanel, #panel{id=joinpanel, body=[
                #span{class= <<"hint center">>,body= <<"Successfully! Here is U'r password, remember this:">>},
                #span{class= <<"clipboard center">>, body=wf:html_encode(wf:f("~s",[User#user3.password]))}
            ]});
        Err -> wf:info(?MODULE, "Error joining: ~p", [Err])
    end;
event(terminate) -> skip;

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