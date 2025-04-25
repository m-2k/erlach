-module(erlach_ban).
-author('Andy').
-compile(export_all).

-include("erlach.hrl").

ip(Req) ->
    {X,_}=cowboy_req:header(<<"x-real-ip">>, Req),
    X.

check(ip,Req) ->
    case wf:config(erlach, proxy_header_x_real_ip, false) of
        true ->
            IP=ip(Req),
            wf:info(?MODULE,"checking ip ~p", [IP]),
            case mnesia:dirty_read(bastard, {ip,IP}) of
                [#bastard{expire=_Exp}|_]=B -> false; % TODO: exire
                [] -> ok
            end;
        _ ->
            wf:info(?MODULE,"check IP skipped (analysis X-REAL-IP header disabled by config)"),
            ok
    end;
check({attachment,hash,Path},Req) ->
    wf:info(?MODULE,"Attachment hash: ~p",[Req]),
    case file:read_file(Path) of
        {ok,Data} ->
            Hash=erlach_utils:hash(Data),
            case mnesia:dirty_read(bastard, {attachment,{hash,Hash}}) of
                [#bastard{expire=_Exp}|_]=B ->
                    IP=ip(Req),
                    B2=ban({ip,IP},{erlang:timestamp(),<<"Autoban due to shit-posting">>,{attachment,{hash,Hash}}},infinity),
                    wf:warning(?MODULE,"Banned: ~p",[B2]),
                    false;
                [] ->
                    ok
            end;
        {error,enoent} ->
            wf:error(?MODULE,"File not found: ~p",[Path]),
            false
    end.

ban(Id,R,E) ->
    B2=case mnesia:dirty_read(bastard, Id) of
        [#bastard{reason=RR}=B] ->
            R2=lists:usort([R|RR]),
            B#bastard{reason=R2,expire=E};
        _ ->
            #bastard{id=Id,created=erlang:timestamp(),reason=[R],expire=E}
    end,
    {atomic,ok}=mnesia:transaction(fun() -> mnesia:write(B2) end),
    B2.

event(#ban{target=attachment=T, value=Id, reason=R, expire=E}=B) ->
    wf:info(?M,"Ban ~p",[B]),
    case kvs:get(T,Id) of
        {ok,#attachment{hash=H}} when is_binary(H) ->
            ban({T,{hash,H}},R,E),
            wf:info(?M,"Banned ~p",[B]),
            ok;
            
        Err ->
            wf:error(?M,"Ban error ~p ~p",[Err, B])
    end;
event(Unknown) -> ?EVENT_ROUTER:unknown(?M,Unknown).