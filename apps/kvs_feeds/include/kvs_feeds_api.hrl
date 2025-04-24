
-type update_fun(_R) :: fun((R2 :: tuple()) -> {ok,R3 :: tuple()} | false).
-type purge_delete_fun(_R) :: fun((R :: tuple()) -> any()).
-type purge_fun() :: fun(() -> list({Container :: atom(), Table :: atom(), Fun :: purge_delete_fun(_)})).
-type eval_fun(_R) :: fun((R2 :: tuple()) -> {ok,any()} | {error,any()}).

-spec kvs_feeds:append(R :: tuple()) -> {ok,tuple()} | {error,any()}.
-spec kvs_feeds:delete(R :: tuple()) -> ok | {error,any()}.
-spec kvs_feeds:update(R :: tuple(), Fun :: update_fun(_R2)) -> {ok,tuple()}.
-spec kvs_feeds:relink(R :: tuple()) -> {ok,tuple()} | {error,any()}.
-spec kvs_feeds:purge(R :: tuple(), Fun :: purge_fun()) -> any().
-spec kvs_feeds:eval(R :: tuple(), Fun :: eval_fun(_R2)) -> {ok,any()} | {error,any()}.