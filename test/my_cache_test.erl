-module(my_cache_test).
-compile(export_all).
-include_lib("eunit/include/eunit.hrl").

-import(my_cache, [create/1, insert/4, lookup/2, delete_obsolete/1]).

create_test_() ->
    N = tt1,
    ?_assert(create(N) =:= ok).

insert_test_() ->
    N = tt1,
    ?_assert(insert(N, "Key1", "Value1", 0) =:= ok),
    ?_assert(insert(N, "Key2", "Value2", 2) =:= ok),
    ?_assert(insert(N, "Key3", "Value3", 60) =:= ok).

lookup_test_() ->
    N = tt1,
    ?_assert(lookup(N, "Key1") =:= {error, undefined}),
    ?_assert(lookup(N, "Key2") =:= {ok, "Value2"}),
    timer:sleep(4 * 1000),
    ?_assert(lookup(N, "Key2") =:= {error, undefined}),
    ?_assert(lookup(N, "Key3") =:= {ok, "Value3"}).

delete_obsolete_test_() ->
    N = tt1,
    ?_assert(delete_obsolete(N) =:= ok).
