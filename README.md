# A ranking list implemented with skiplist in Erlang API.

## Build

```
make all
```

## Test

```
erl
1> test:test().

2> test:test1().
create time = 1.3828277587890625e-5
insert time = 0.20737981796264648
delete time = 0.27568984031677246
get_rank_range time = 0.2973809242248535
get_rank_range time = 0.32408595085144043
check_same ret=same, T1 T2 = 0.3566708564758301
get_score_range T3 T4 time = 0.36693787574768066
get_score_range T3 T4 time = 0.38155579566955566
check_same ret=same, T3 T4 time = 0.3842628002166748
get_rank_range = ["5","4","3","2"]
get_rank_range = ["2","3","4","5"]
end time = 0.3845968246459961
get_score_range = ["20","19","18","17","16","15","14","13","12","11","10"]
get_score_range = ["10","11","12","13","14","15","16","17","18","19","20"]
end time = 0.3848419189453125
delete_by_rank = ["15","14","13","12","11","10"]
end time = 0.3853938579559326
destroy = ok
end time = 0.38797688484191895
ok
```
