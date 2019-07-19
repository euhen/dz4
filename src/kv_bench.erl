%% coding: utf-8
%2. Провести сравнительный анализ всех способов хранения данных в режиме
%   ключ/значение: maps, proplists, dict, process dictionary, ets

-module(kv_bench).
-compile(export_all).

start() ->
    io:fwrite("\nETS:\n"),
    Ets = init_ets(),
    EtsW = bench(save_ets, Ets),
    EtsR = bench(read_ets, Ets),
    EtsD = bench(drop_ets, Ets),
    io:format("Write: ~f mics~n"
        "Read: ~f mics~n" "Delete: ~f mics~n", [EtsW, EtsR, EtsD]),
    
    io:fwrite("\nProcess Dictionary:\n"),
    PdW = bench(save_pd, undefined),
    PdR = bench(read_pd, undefined),
    io:format("Write: ~f mics~n" "Read: ~f mics~n", [PdW, PdR]),
    
    io:fwrite("\nDict:\n"),
    Dict = init_dict(),
    DictW = bench(save_dict, Dict),
    DictR = bench(read_dict, Dict),
    io:format("Write: ~f mics~n" "Read: ~f mics~n", [DictW, DictR]),
    
    io:fwrite("\nMaps get/put:\n"),
    Mg = init_map_get(),
    MgW = bench(save_map_get, Mg),
    MgR = bench(read_map_get, Mg),
    io:format("Write: ~f mics~n" "Read: ~f mics~n", [MgW, MgR]),
    
    io:fwrite("\nMaps matching:\n"),
    Mm = init_map_match(),
    MmW = bench(save_map_match, Mm),
    MmR = bench(read_map_match, Mm),
    io:format("Write: ~f mics~n" "Read: ~f mics~n", [MmW, MmR]),
    
    io:fwrite("\nProplists\n"),
    List = init_list(),
    PlW = bench(save_list, List),
    PlR = bench(read_list, List),
    io:format("Write: ~f mics~n" "Read: ~f mics~n", [PlW, PlR]),
    
    %R = bench(test, undefined),
    %io:fwrite("Hello, world!\n"),
    %io:fwrite("~p~n",[ R ]),
    io:fwrite("\nFinished\n").

test(_X, _Y) -> lists:seq(1, 1000).

init_ets() -> ets:new(my_table, [named_table]).
save_ets(K, my_table) -> ets:insert(my_table, {K, {K, "test1"}}).
read_ets(K, my_table) -> ets:lookup(my_table, K) =/= [].
drop_ets(K, my_table) -> ets:delete(my_table, K).

save_pd(K, _) -> put(K, "test1").
read_pd(K, _) -> get(K) =/= undefined.

init_dict() -> dict:new().
save_dict(K, D) -> dict:store(K, {K, "test1"}, D).
read_dict(K, D) -> dict:find(K, D) =/= error.

init_map_get() -> maps:new().
save_map_get(K, M) -> maps:put(K, {K, "test1"}, M).
read_map_get(K, M) -> maps:get(K, M, undefined) =/= undefined.

init_map_match() -> #{}.
save_map_match(K, M) -> M#{K => {K, "test1"}}.
read_map_match(K, M) -> case M of #{K := _} -> true; _ -> false end.

init_list() -> [].
save_list(K, L) -> [{{K, "test1"}}|L].
read_list(K, L) -> proplists:get_value(K, L) =/= undefined.


bench(FunName, StorageInstance) ->
    AL = [[X, Y] || X <- lists:seq(1, 10000), Y <- [StorageInstance]],
    test_avg(?MODULE, FunName, AL).

test_avg(M, F, AL) ->
    L = test_loop(M, F, AL, []),
    Length = length(L),
    lists:foldl(fun(X, Sum) -> X + Sum end, 0, L) / Length.

test_loop(_M, _F, [], List) ->
    List;
test_loop(M, F, [A|R], List) ->
    {T, _Result} = timer:tc(M, F, A),
    test_loop(M, F, R, [T|List]).

