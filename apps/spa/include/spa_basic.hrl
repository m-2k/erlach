-ifndef(SPA_BASIC_HRL).
-define(SPA_BASIC_HRL, true).

-define(M,?MODULE).
-define(UNDEF,undefined).
-define(RECORD_MANAGER_API, [new/1, set/3, get/2, channel/1]).

%%% Render Events
-define(RDR_EV(Render), render=Render, target, scope).
-define(RDR_EV, ?RDR_EV(?UNDEF)).
-record(rdr_ev, { ?RDR_EV }).

-define(REC(R,Tag), is_tuple(R) andalso element(1,R) =:= Tag).
-endif.