-ifndef(SPA_HRL).
-define(SPA_HRL, true).

-include("spa_basic.hrl").

-define(SPA_CONFIG_NAME,spa).
-define(STATE,state).
-define(NOTIFY_PANEL,'popup-messages').

-define(RM,spa_record_manager).

-ifndef(RDR_PUBSUB_RENDER).
-define(RDR_PUBSUB_RENDER, ?UNDEF). % page_id
-endif.
-record(render_event, { ?RDR_EV, event }).
-record(pubsub,       { ?RDR_EV, action, element, data, into = top, meta, from=self() }). % into :: top | bottom
-record(view,         { ?RDR_EV, element, level, option, partial = false, start, count, feed, table, direction }).
-record(create,       { ?RDR_EV, panel, feed }).
-record(edit,         { ?RDR_EV, id }).
-record(cancel,       { ?RDR_EV, panel, feed }).
-record(update,       { ?RDR_EV, id }).
-record(delete,       { ?RDR_EV, id, value, store }).
-record(add,          { ?RDR_EV, panel, feed, forms, into, store }).
-record(put,          { ?RDR_EV, id, forms, store }).
-record(subscribe,    { ?RDR_EV(?RDR_PUBSUB_RENDER), uid }).
-record(unsubscribe,  { ?RDR_EV(?RDR_PUBSUB_RENDER), uid }).

-ifndef(QUERY_EXT).
-define(QUERY_EXT, q1).
-endif.

-record(query,{ option, ?QUERY_EXT }).

-ifndef(ROUTE_EXT).
-define(ROUTE_EXT, page). % page_id
-endif.

-record(route, {
    query :: #query{},
    module :: atom(),
    render :: atom(),
    level :: binary(),
    option :: any(),
    ?ROUTE_EXT
    }).

-record(postback,{
    action = view :: atom(),
    query :: #query{},
    history = true :: boolean(),
    route_option :: any()
    }).

-ifndef(STATE_EXT).
-define(STATE_EXT, view = []).
-endif.

-record(st,{
    route :: #route{},
    action = view,
    user,
    level,
    ?STATE_EXT
    }).
    
-ifndef(HES_EXT).
-define(HES_EXT, target).
-endif.

-record(hes,{
    option,
    scope,
    cache, % #map{}
    stack, % elements
    ?HES_EXT
    }).

-record(compose,{
    panel,
    element,
    forms = [] :: list(),
    scope,
    cache,
    state,
    body = [],
    guard = fun() -> compose end :: function(),
    button = fun spa_render:button/2 :: function(),
    id = fun spa:ei/2 :: function(), %% TODO: fix for spa_render/spa_event
    into
    }).
    
-record(static,{render, level}).

-endif.
