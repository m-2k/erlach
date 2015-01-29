-module(root). %% MAIN PAGE / BOARDS LISTING
-vsn('0.0.0').

-compile(export_all).
-include_lib("n2o/include/wf.hrl").
-include_lib("kvs/include/kvs.hrl").
-include_lib("kvs/include/feed.hrl").
-include_lib("db/include/post.hrl").
-include_lib("db/include/attachment.hrl").

-include_lib("db/include/board.hrl").
-include_lib("db/include/user.hrl").
-include("erlach.hrl").

% -ifndef(SESSION).
% -define(SESSION, (wf:config(n2o,session,erlach_session))).
% -endif.

peer()    -> io_lib:format("~p",[wf:peer(?REQ)]).
main()    -> #dtl{file="erlach",app=review,bindings=[{body,body()}, {theme,<<"glassy">>}, {title,<<"Erlach">>}]}. %html:page(<<"Erlach">>,body(),glassy).
body() ->
	% Session = ?CTX#cx.session,
	% User = case wf:user() of
	% 	#user3{id=UID} -> UID;
	% 	U -> U
	% end,
	% timer:sleep(1000),
	% User2 = u:ensure_user(),
	
	% UserName = case User2 of
	% 	#user3{id={?T, _ID}, name=N} -> undefined;
	% 	#user3{name=N} -> N
	% end,
	
	% wf:wire(#transfer{ state={undefined_state_init} }),
	% wf:wire(#transfer{ events = {server, {temp_user_created,undefined}} }),
	
	
    % wf:info(?MODULE, "body ~p", [User]),
    Boards = lists:map(fun(#board{ id=Id, uri=Uri, name=Name }) ->
        #panel{ class = <<"board">>, body = [
            #link{ class= <<"button girl alpha">>, href = wf:f("/board?board=~p", [Id]), body = [
				#span{class= <<>>,body=wf:f("~s", [Uri])},
				wf:html_encode(wf:f(" - ~s", [Name]))] }
        ]}
        end, kvs:all(board)),
    % {ok, Svg} = file:read_file("apps/review/priv/static/svg/erlach-board-list-3.svg"),
    % wf:info(?MODULE, "~p", [file:get_cwd()]),
	% wf:wire(#event{target="logout", postback=logout, type=click, source=[]}),
	% "<svg style='width:20px; fill:red;'><use xlink:href='#ei-arrow-right-icon'></use></svg>",	
	html:body(#panel{ class= <<"board-list">>, body=
			% #span{ body=wf:f("Session: ~p<br/>User: ~p<br/>User 2: ~p", [Session,User,User2])},
			% [Boards,#span{ class= <<"helo">>, body = <<"Coming soon!">>}]}).
			Boards}).
			%"<svg xmlns='http://www.w3.org/2000/svg' version='1.1'><defs><filter id='blur'><feGaussianBlur stdDeviation='5'/></filter></defs></svg>"]},

event(init) ->
	% self() ! {server, "ololo"},
	% self() ! {custom, "ololo"},
	% self() ! {server, "ololo", 3},
	% self() ! {server, {temp_user_created,undefined}},
	wf:info(?MODULE, "User: ~p WS Pid: ~p, State ~p", [wf:user(), self(), get(state)]);
% event(join) ->
% 	u:join(),
% 	wf:info(?MODULE, "User: ~p", [wf:user()]);
event({server, {temp_user_created,User}}) ->
	wf:info(?MODULE, "SERVER EVENT User: ~p WS Pid: ~p, State ~p", [u:id(User), self(), get(state)]),
	html:info("Clear browser cache if U have problem"),
	html:warning("Development version");
event({server, Message}) -> % OK
	wf:info(?MODULE, "Server message: ~p", [Message]);
event({server, Message, M}) ->
	wf:info(?MODULE, "Server3 message: ~p ~p", [Message,M]); %% NOT WORK, USE {server, Val}
event({custom, Message}) ->
	wf:info(?MODULE, "Custom message: ~p", [Message]);  %% NOT WORK, USE {server, Val}
event(terminate) -> skip;
event(Event) -> guard:shared_event(?MODULE, Event).

event(#ev{msg={chat,Pid}},#cx{req=_Req}=Cx) -> Pid ! {peer(), wf:qp(message,Cx)};
event(_Event,_) -> skip.


c() ->
    add_new_board(p, <<"Photo">>, <<"Фотография"/utf8>>, 1, false),
    add_new_board(g, <<"Girls">>, <<"Девушки"/utf8>>, 1, false),
	add_new_board(c, <<"Cinema">>, <<"Фильмы"/utf8>>, 1, false),
	add_new_board(ps, <<"Psychology">>, <<"Психология"/utf8>>, 1, false),
	add_new_board(po, <<"Poetry">>, <<"Стихи"/utf8>>, 1, false),
	add_new_board(pr, <<"Prank">>, <<"Пранк"/utf8>>, 1, false),
	add_new_board(ee, <<"Electrical engineering">>, <<"Электротехника"/utf8>>, 1, false).

add_name(Uid,Name) ->
	kvs:add(#name{
    id=kvs:next_id(name,1),
	feed_id={user,Uid},
	displayname=Name,
	created=erlang:now()
    }).

add_new_board(Uri, Name, Description, GroupID, Hidden) ->
    kvs:add( #board {
        id=kvs:next_id(board, 1),
        created=erlang:now(),
        uri=Uri,
        name=Name,
        description=Description,
        hidden=Hidden,
        feed_id={group, GroupID}
        }).