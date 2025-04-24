-module(eauth_user).
-author('Andy').

-include("eauth.hrl").
-include("eauth_user.hrl").

-include_lib("kvs/include/group.hrl").
-include_lib("kvs/include/metainfo.hrl").

-compile(export_all).

metainfo() ->
    #schema{name=kvs,tables=[
        #table{name=group,container=feed,fields=record_info(fields,group)},
        #table{name=user,container=feed,fields=record_info(fields,user),
                 keys=[email,facebook_id,googleplus_id,twitter_id,github_id]}
    ]}.


% From database
get() ->
    case wf:user() of
        Uid when is_integer(Uid) -> case kvs:get(user,Uid) of {ok,U} -> U; _ -> ?UNDEF end;
        _ -> ?UNDEF
    end.

put(User) ->
    user(User).

state() -> % get state
    case wf:config(eauth,function_state) of
        {M,F} -> M:F();
        _ -> ok
    end. 
state(State) -> % write state
    case wf:config(eauth,function_state) of
        {M,F} -> M:F(State);
        _ -> ok
    end.
state_user(State) -> % get user from state
    case wf:config(eauth,function_state_user) of
        {M,F} -> M:F(State);
        _ -> erlang:get({?M,user})
    end.
state_user(State,User) -> % put user to state
    case wf:config(eauth,function_state_user) of
        {M,F} -> M:F(State,User);
        _ -> erlang:put({?M,user},User)
    end.

% From state
user(#user{}=U) -> state(state_user(state(),U)), U;
user(State) -> state_user(State).
user() -> user(state()).

uid(#user{id=Id}) -> Id;
uid(State) -> uid(state_user(State)).
uid() -> uid(state()).

% for event
is_alive() -> is_alive(user()).
% for render
is_alive(#user{deleted=false}) -> true;
is_alive(_) -> false.
