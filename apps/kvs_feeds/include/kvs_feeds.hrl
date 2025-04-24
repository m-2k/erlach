-ifndef(KVS_FEEDS_HRL).
-define(KVS_FEEDS_HRL, "kvs_feeds.hrl").

-include_lib("kvs/include/kvs.hrl").
-include_lib("kvs/include/feed.hrl").

-define(M,?MODULE).
-define(UNDEF,undefined).

-define(CONFIG(Key,Default), application:get_env(kvs_feeds,Key,Default)).
-define(LOG(S,A),case ?CONFIG(log,{wf,info}) of {M,F} -> M:F(?M,S,A); _ -> skip end).

-endif.
