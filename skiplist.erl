-module(skiplist).

-export([
        create/1,
        destroy/1,
        insert/3,
        delete/3,
        delete_by_rank/3,
        get_count/1,
        get_rank/3,
        get_rank_range/3,
        get_score_range/3,
        get_member_by_rank/2,
        dump/1,
        dump_all/0
    ]).

-on_load(load/0).

load() ->
    erlang:load_nif("./skiplist", 0).

create(_RankId) ->
    erlang:nif_error(undef).

destroy(_RankId) ->
    erlang:nif_error(undef).

insert(_RankId, _Score, _Name) ->
    erlang:nif_error(undef).

delete(_RankId, _Score, _Name) ->
    erlang:nif_error(undef).

delete_by_rank(_RankId, _Start, _End) ->
    erlang:nif_error(undef).

get_count(_RankId) ->
    erlang:nif_error(undef).

get_rank(_RankId, _Score, _Name) ->
    erlang:nif_error(undef).

get_rank_range(_RankId, _R1, _R2) ->
    erlang:nif_error(undef).

get_score_range(_RankId, _S1, _S2) ->
    erlang:nif_error(undef).

get_member_by_rank(_RankId, _R) ->
    erlang:nif_error(undef).

dump(_RankId) ->
    erlang:nif_error(undef).

dump_all() ->
    erlang:nif_error(undef).