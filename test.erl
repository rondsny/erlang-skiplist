-module(test).

-export([test/0,
    test1/0]).

unixtime() ->
    {MS, S, Mis} = os:timestamp(),
    MS*1000000 + S * 1 + Mis / 1000000.

check_same([A|Acc1], [A|Acc2]) ->
    check_same(Acc1, Acc2);
check_same([], []) ->
    same;
check_same(_A, _B)->
    not_same.

test() ->
    RankId = 1001,
    skiplist:create(RankId),
    skiplist:insert(RankId, 10.0, "a1"),
    skiplist:insert(RankId, 30.0, "a2"),
    skiplist:insert(RankId, 20.0, "a3"),
    skiplist:insert(RankId, 35.0, "a4"),
    skiplist:insert(RankId, 23.0, "a5"),
    skiplist:insert(RankId, 123.0, "a6"),
    skiplist:insert(RankId, 123.0, "a7"),
    skiplist:insert(RankId, 123.0, "a8"),
    skiplist:dump(RankId),
    io:format("---------------------------------------\n", []),
    io:format("--delete_by_rank = ~p ------------\n", [skiplist:delete_by_rank(RankId, 2, 5)]),
    skiplist:dump(RankId),

    RankId2 = 1002,
    skiplist:create(RankId2),
    skiplist:insert(RankId2, 10.0, "b1"),
    skiplist:insert(RankId2, 30.0, "b2"),
    skiplist:insert(RankId2, 20.0, "b3"),
    skiplist:dump(RankId2),
    io:format("---------------------------------------\n", []),

    skiplist:delete(RankId2, 20.0, "b3"),
    skiplist:dump(RankId2),

    RankId3 = 1003,
    skiplist:create(RankId3),
    skiplist:insert(RankId3, 10.0, "b1"),
    skiplist:insert(RankId3, 30.0, "b2"),
    skiplist:insert(RankId3, 20.0, "b3"),
    skiplist:dump(RankId3),
    io:format("-- get_rank = ~p--------------\n", [skiplist:get_rank(RankId3, 20.0, "b3")]),
    io:format("-- get_rank = ~p--------------\n", [skiplist:get_rank(RankId3, 10.0, "b1")]),
    io:format("-- get_rank = ~p--------------\n", [skiplist:get_rank(RankId3, 30.0, "b2")]),
    io:format("-- get_rank = ~p--------------\n", [skiplist:get_rank(RankId3, 40.0, "b4")]),

    io:format("-- get_rank_range = ~p--------------\n", [skiplist:get_rank_range(RankId3, 1, 2)]),
    io:format("-- get_score_range = ~p--------------\n", [skiplist:get_score_range(RankId3, 15.0, 35.0)]),
    io:format("-- get_member_by_rank = ~p--------------\n", [skiplist:get_member_by_rank(RankId3, 2)]),

    io:format("-- -- -------\n", []),
    io:format("-- -----count = ~p-- --\n", [skiplist:get_count(RankId3)]),
    skiplist:delete(RankId3, 20.0, "b3"),
    io:format("-- -----count = ~p-- --\n", [skiplist:get_count(RankId3)]),
    skiplist:delete(RankId3, 20.0, "b3"),
    skiplist:destroy(RankId3),


    RankId4 = 1004,
    skiplist:create(RankId4),
    skiplist:insert(RankId4, 10.0, "b1"),
    skiplist:insert(RankId4, 30.0, "b2"),
    skiplist:insert(RankId4, 20.0, "b3"),
    skiplist:dump(RankId4),

    io:format("------------------dump all---------------------\n", []),
    skiplist:dump_all(),
    ok.


test1() ->
    Time1 = unixtime(),

    RankId = 2001,
    skiplist:create(RankId),
    io:format("create time = ~p \n", [unixtime()-Time1]),

    [skiplist:insert(RankId, I+0.0, integer_to_list(I)) || I <- lists:seq(1, 100000)],
    [skiplist:insert(RankId, I+0.0, integer_to_list(I)) || I <- lists:seq(1, 100000)],
    io:format("insert time = ~p \n", [unixtime()-Time1]),
    [skiplist:delete(RankId, I+0.0, integer_to_list(I)) || I <- lists:seq(1, 100000)],
    io:format("delete time = ~p \n", [unixtime()-Time1]),
    T1 = skiplist:get_rank_range(RankId, 100, 100000),
    io:format("get_rank_range time = ~p \n", [unixtime()-Time1]),
    T2 = skiplist:get_rank_range(RankId, 100000, 100),
    io:format("get_rank_range time = ~p \n", [unixtime()-Time1]),
    io:format("check_same ret=~p, T1 T2 = ~p \n", [check_same(T1, lists:reverse(T2)), unixtime()-Time1]),

    T3 = skiplist:get_score_range(RankId, 100.0, 100000.0),
    io:format("get_score_range T3 T4 time = ~p \n", [unixtime()-Time1]),
    T4 = skiplist:get_score_range(RankId, 100000.0, 100.0),
    io:format("get_score_range T3 T4 time = ~p \n", [unixtime()-Time1]),
    io:format("check_same ret=~p, T3 T4 time = ~p \n", [check_same(T3, lists:reverse(T4)), unixtime()-Time1]),

    io:format("get_rank_range = ~p \n", [skiplist:get_rank_range(RankId, 2, 5)]),
    io:format("get_rank_range = ~p \n", [skiplist:get_rank_range(RankId, 5, 2)]),
    io:format("end time = ~p \n", [unixtime()-Time1]),

    io:format("get_score_range = ~p \n", [skiplist:get_score_range(RankId, 10.0, 20.0)]),
    io:format("get_score_range = ~p \n", [skiplist:get_score_range(RankId, 20.0, 10.0)]),
    io:format("end time = ~p \n", [unixtime()-Time1]),

    io:format("delete_by_rank = ~p \n", [skiplist:delete_by_rank(RankId, 15, 10)]),
    io:format("end time = ~p \n", [unixtime()-Time1]),

    io:format("destroy = ~p \n", [skiplist:destroy(RankId)]),
    io:format("end time = ~p \n", [unixtime()-Time1]),

    ok.