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
main()    ->
	% wf:info(?MODULE, "DEBUG:~p", ['3-2']),
	% wf:info(?MODULE,"Peer: ~p",[wf:peer(?REQ)]),
	R = avz:callbacks([twitter]),
	% wf:info(?MODULE, "DEBUG:~p ~p", ['3-1', R]),
	#dtl{file="erlach",app=review,bindings=[{body,body()}, {title,<<"Profile">>}]}.
	
names_list() ->
	#panel{class= <<"flex-vertical">>, body = [
		[#button{class= <<"primary">>, body= <<"Add"/utf8>>, postback={showform, addname}}] ++ lists:map(fun(N) ->
			#span{class= <<"square">>, body= <<"", (guard:html_escape(N#name.displayname))/binary>>}
			end, u:names()) ]}.

body() ->
	% wf:info(?MODULE, " body wf:user/0: ~p", [wf:user()]),
	% Session = ?CTX#cx.session,
	% User2 = u:ensure_user(),
	% wf:info(?MODULE, "body...", []),
	% UserName = case User2 of
	% 	#user3{id={?T, _ID}, name=N} -> undefined;
	% 	#user3{name=N} -> N
	% end,
	
	% wf:info(?MODULE, " body action: ~p", [wf:qs(<<"action">>)]),
	% ActionHtml = case {wf:qs(<<"action">>), wf:user()} of
	% 	{<<"auth">>, _User} ->
	% 		#panel{body=[
	% 			#span{body= <<"Enter passphrase for authorization">>},
	% 			#textbox{id=passphrase,autofocus=true,placeholder= <<"Passphrase">>},
	% 			#button{id=auth, class= <<"success center">>, body = <<"Go!">>, postback=auth, source=[passphrase]}
	% 		]};
	% 	{<<"twitter-oauth">>, _User} ->
	% 		#panel{body=[
	% 			% #span{body= <<"Enter twitter password:">>},
	% 			% #textbox{id=passphrase,autofocus=true,placeholder= <<"Passphrase">>},
	% 			avz:buttons([twitter])
	% 		]};
	% 	{<<"join">>, #user3{id={?T,_ID}}} ->
	% 		% wf:wire(#event{target="join", postback=join, type=click, source=[]}),
	% 		#panel{id=joinpanel, body=[
	% 			#span{class= <<"hint center">>,body= <<"If you want to join Erlach just do it with this button">>},
	% 			#button{id=join, class= <<"success center">>, body = <<"Join!">>, postback=join}
	% 		]};
	% 	{<<"join">>, _User} -> wf:redirect("/profile"), [];
	% 	_ -> #span{body= <<"Names: ">>}
	% end,
	Content = case ?SESSION:erase_param(?MODULE) of
		signin -> #button{body= <<"Auth with Twitter">>, class = <<"success center">>, postback={twitter,logintwitter}};
		_ -> #panel{class = <<"flex-horizontal">>, body=[
			names_list(),
			html:username_edit()]}
	end,

    % wf:info(?MODULE, " body wf:user/0: ~p,~nUser2: ~p", [wf:user(),User2]),
    % html:body(ActionHtml).
	
	html:body([ #span{body= wf:f("User: ~p", [u:get()])}, Content]).

event(init) ->
	wf:info(?MODULE, " init wf:user/0: ~p", [wf:user()]),
	ok;
event({showform, addname}) ->
	wf:update(<<"username-manage">>, html:username_add());
event({username,add}) ->
	html:info("SASAY LALKA reg" ++ wf:temp_id()),
	html:success("SASAY LALKA reg" ++ wf:temp_id()),
	html:warning("SASAY LALKA reg" ++ wf:temp_id()),
	html:error("SASAY LALKA reg" ++ wf:temp_id());
event(join) ->
	wf:info(?MODULE, "Joining...", []),
	case u:join() of
		{ok, User} ->
			wf:info(?MODULE, "User joined: ~p", [User]),
			wf:update(joinpanel, #panel{id=joinpanel, body=[
				#span{class= <<"hint center">>,body= <<"Successfully! Here is U'r password, remember this:">>},
				#span{class= <<"clipboard center">>, body=wf:html_encode(wf:f("~s",[User#user3.password]))}
			]});
		Err -> wf:info(?MODULE, "Error joining: ~p", [Err])
	end,
	% wf:redirect("/profile"),
	wf:info(?MODULE, "User: ~p", [wf:user()]);
% event(auth) ->
% 	wf:info(?MODULE, "Auth... passphrase: ~p", [wf:to_binary(wf:q(passphrase))]),
% 	R = case kvs:index(user3, password, wf:to_binary(wf:q(passphrase))) of
% 		[] -> {error, not_found};
% 		[#user3{}=U] -> {ok, U};
% 		[U] -> {error, {unknown_record,U}};
% 		[H|T] -> {error, {several_values,[H|T]}};
% 		Err -> {error, Err}
% 	end,
% 	case R of
% 		{ok, _User} -> wf:info(?MODULE, "Auth OK!", []);
% 		{error, Reason} -> wf:warning(?MODULE, "Auth error: ~p", [Reason])
% 	end, ok;
% event(logout) ->
% 	u:logout(),
% 	wf:redirect("/profile");
	% wf:info(?MODULE, "User: ~p", [wf:user()]);
event(terminate) -> skip;

% exist
event({login, twitter, #token{user=UserID}, _Args}) ->
	% User = u:ensure_user(),
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

% event(X) 		 -> avz:event(X).
event(Event) -> guard:shared_event(?MODULE, Event).

% api_event(X,Y,Z) -> avz:api_event(X,Y,Z).

% event(Event) ->
%     wf:info(?MODULE, "Unknown event: ~p", [Event]),
%     skip.
event(#ev{msg={chat,Pid}},#cx{req=_Req}=Cx) -> Pid ! {peer(), wf:qp(message,Cx)};
event(_Event,_) -> skip.
