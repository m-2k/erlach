# KVS Feeds
**Generic KVS feed-server**

## Overview

Feed server for KVS abstract database middleware.

Implements database queries in the form of atomic operations or in other words, provides transaction for KVS-feeds logic.

## Dependency

* [kvs](https://github.com/synrc/kvs) – Database middleware.
* [n2o](https://github.com/synrc/n2o) – n2o_async needed.

## Configuration

### sys.config

* `log :: {Mod,Fun}` – Custom logging `function/3`, default: `{wf,info}`
* `feed_timeout :: non_neg_integer()` – Pool worker timeout, in ms, default: `600000` (10 min)
* `feed_timeout_action :: hibernate | stop` – Action for inactive pool workers, default: hibernate

## Api

* `append(Record)` – Add `Record` to database.
* `delete(Record)` – Remove `Record` from database.
* `update(Record,Fun)` – Update `Record` with `Fun/1` which takes `Record2` (at the time of execution) and should return `{ok,Record3}` (modified) if surgery is to be performed, or anything else to cancel.
* `relink(Record)` – Bring `Record` to top in `Record` feed.
* `purge(Record,Fun)` – Remove `Record` and related feeds with `Fun/0` which should return a list of feeds, example: `kvs_feeds:purge(E,fun() -> [{feed,attachments,fun kvs_feed:delete/1}] end)`, `feed_id` to be deleted will be generated as `{attachment,element(2,E)}` for `feed` container table.
* `eval(Record,Fun)` – Eval custom `Fun/1` which takes `Record2` (at the time of execution), like as `update/2`.

## Usage

```erlang
R=#post{},
{ok,R2}=kvs_feeds:append(R),
Link = 1000,
FunUpdate=fun(#post{links=Links}=P) -> {ok,P#post{links=lists:usort([Link | Links])}} end,
{ok,R3}=kvs_feeds:update(R2,FunUpdate),
     ok=kvs_feeds:delete(R3),
        kvs_feeds:purge(R3,fun() -> [{feed,attachment,fun kvs_feeds:delete/1}] end),
```
