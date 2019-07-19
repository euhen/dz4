%% coding: utf-8
%1. Написать библиотеку для кэширования

-module(my_cache).
-export([create/1]).
-export([insert/4]).
-export([lookup/2]).
-export([delete_obsolete/1]).
-include_lib("stdlib/include/ms_transform.hrl").

-define(SECOND, 1).

create(TableName) ->
    TableName = ets:new(TableName, [named_table]),
    ok.

insert(TableName, Key, Value, MaxTime) ->
    ExpTime = MaxTime + erlang:system_time(?SECOND),
    true = ets:insert(TableName, {Key, Value, ExpTime}),
    ok.

lookup(TableName, GivenKey) ->
    Now = erlang:system_time(?SECOND),
    MatchSpec = ets:fun2ms(
        fun({Key, Value, ExpTime})
            when ExpTime > Now, Key =:= GivenKey ->
                Value
        end
    ),
    case ets:select(TableName, MatchSpec) of
        [Value] -> {ok, Value};
        [] -> {error, undefined}
    end.

delete_obsolete(TableName) ->
    Now = erlang:system_time(?SECOND),
    MatchSpec = ets:fun2ms(
        fun({_Key, _Value, ExpTime})
            when ExpTime =< Now ->
                ok
        end
    ),
    ets:select_delete(TableName, MatchSpec),
    ok.

