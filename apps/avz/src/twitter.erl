-module(twitter).
-author('Andrii Zadorozhnii').
-include_lib("n2o/include/wf.hrl").
-include_lib("avz/include/avz.hrl").
-include_lib("kvs/include/user.hrl").

-include_lib("db/include/user.hrl").

-compile(export_all).
-export(?API).
-define(CONSUMER_KEY, case application:get_env(avz, tw_consumer_key) of {ok, K} -> K;_-> "" end).
-define(CONSUMER_SECRET, case application:get_env(avz, tw_consumer_secret) of {ok, S} -> S; _-> "" end).
-define(CONSUMER, {?CONSUMER_KEY, ?CONSUMER_SECRET, hmac_sha1}).

registration_data(Props, twitter, Ori)->
	% wf:info(?MODULE, "registration_data. Props:~p~n",[Props]),
    Id = proplists:get_value(<<"id_str">>, Props),
    UserName = binary_to_list(proplists:get_value(<<"screen_name">>, Props)),
    Email = email_prop(Props,twitter),
    Ori#user3{  id = kvs:next_id(user3, 1),
                name = re:replace(UserName, "\\.", "_", [{return, list}]),
                email = Email,
                tokens = [{twitter,Id}|Ori#user3.tokens],
                created = erlang:now() }.

token_prop(Props, twitter) -> proplists:get_value(<<"id_str">>, Props).
email_prop(Props, twitter) -> proplists:get_value(<<"screen_name">>, Props).

callback() ->
    Token = wf:q(<<"oauth_token">>),
    Verifier =wf:q(<<"oauth_verifier">>),
	case {u:is_temp(), Token, Verifier} of
		{_, undefined, _} -> skip;
		{_, _, undefined} -> skip;
		{true, _, _} ->
			case get_access_token(binary_to_list(Token), binary_to_list(Verifier)) of
				not_authorized -> skip;
				Props -> UserData = show(Props), avz:login(twitter, UserData#struct.lst)
			end;
		_ -> skip
	end.

login_button() -> #panel{class=["btn-group"], body=
    #link{id=twlogin, class=[btn, "btn-info", "btn-large", "btn-lg"],
        body=[#i{class=[fa,"fa-twitter","fa-lg","icon-twitter", "icon-large"]}, <<"Twitter">>],
        postback={twitter,logintwitter}}}.

sdk() -> [].
api_event(_,_,_) -> ok.
event({twitter,logintwitter}) ->
    case get_request_token() of
         {RequestToken, _, _} -> wf:redirect(authenticate_url(RequestToken));
         {error, R} -> error_logger:info_msg("Twitter request failed:", [R]), [] end.

get_request_token()->
  URL = "https://api.twitter.com/oauth/request_token",
  case oauth:get(URL, [], ?CONSUMER) of
    {ok, Response} ->
      Params = oauth:params_decode(Response),
      RequestToken = oauth:token(Params),
      RequestTokenSecret = oauth:token_secret(Params),
      CallbackConfirmed = proplists:get_value("oauth_callback_confirmed", Params),
      {RequestToken, RequestTokenSecret, CallbackConfirmed};
    {error, E}-> {error, E}
  end.

get_access_token(undefined, undefined)-> not_authorized;
get_access_token(undefined, _)-> not_authorized;
get_access_token(_, undefined)-> not_authorized;
get_access_token(Token, Verifier)->
  URL = "https://api.twitter.com/oauth/access_token",
  Signed = oauth:sign("GET", URL, [{"oauth_verifier", Verifier}], ?CONSUMER, Token, ""),
  {OauthParams, QueryParams} = lists:partition(fun({K, _}) -> lists:prefix("oauth_", K) end, Signed),
  Request = {oauth:uri(URL, QueryParams), [oauth:header(OauthParams)]},
  {ok, Response} = httpc:request(get, Request, [{autoredirect, false}], []),
  case Response of
    {HttpResponse, _, _}->
      case HttpResponse of
        {"HTTP/1.1",200,"OK"}->
          Params = oauth:params_decode(Response),
          Params;
        _ -> not_authorized
      end;
    _ -> not_authorized
  end.

authenticate_url(RequestToken)->
    oauth:uri("https://api.twitter.com/oauth/authenticate", [{"oauth_token", RequestToken}]).
authorize_url(RequestToken)->
    oauth:uri("https://api.twitter.com/oauth/authorize", [{"oauth_token", RequestToken}]).

show(Props)->
  URI = "https://api.twitter.com/1.1/users/show.json",
  {ok, Response} = oauth:get(URI, [{"user_id", proplists:get_value("user_id", Props)},
                                   {"include_entities", false}],
                            ?CONSUMER, oauth:token(Props), oauth:token_secret(Props)),
  case Response of
    {HttpResponse, _, Body} -> case HttpResponse of
                                    {"HTTP/1.1", 200, "OK"} ->  n2o_json:decode(Body);
                                    _-> error end;
    _ -> error
  end.

