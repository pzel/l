-module(special_folds_tests).
-include_lib("proper_eunit/include/pt_proper_eunit.hrl").

%%
%%  Special folds
%%

%% concat/1
concat_empty_test() ->
    ?assertEqual([], l:concat("")).
concat_one_test() ->
    ?assertEqual("a", l:concat(["a"])).
concat_two_test() ->
    ?assertEqual("ab", l:concat(["a", "b"])).
concat_three_test() ->
    ?assertEqual("abc", l:concat(["a", "b", "c"])).
prop_concat_lengths() ->
    ?FORALL(Xs, list(non_empty(list(integer()))),
            l:length(l:concat(Xs)) == l:sum(l:map(fun l:length/1, Xs))).

%% concat_map/2
concat_map_empty_test() ->
    ?assertEqual([], l:concat_map(repF(1), [])).
prop_concat_map_id() ->
    ?FORALL(Xs, list(),
            Xs == l:concat_map(repF(1), Xs)).
prop_concat_map_length() ->
    ?FORALL({N,Xs}, {non_neg_integer(), list(integer())},
            l:length(Xs) * N == length(l:concat_map(repF(N), Xs))).

%% and/1
and_badarg_test() ->
    ?assertError(badarg, l:and_([hello, 1, {}])).
and_empty_test() ->
    ?assertEqual(true, l:and_([])).
prop_and_all_true() ->
    ?FORALL(Xs, list(true), true == l:and_(Xs)).
prop_and_at_least_one_false() ->
    ?FORALL({Ts,Fs}, {list(true), non_empty(list(false))},
            false == l:and_(l:append(Ts,Fs))).

%% or/1
or_badarg_test() ->
    ?assertError(badarg, l:or_([hello, 1, {}])).
or_empty_test() ->
    ?assertEqual(false, l:or_([])).
prop_or_all_false() ->
    ?FORALL(Xs, list(false), false == l:or_(Xs)).
prop_or_at_least_one_true() ->
    ?FORALL({Fs,Ts}, {list(false), non_empty(list(true))},
            true == l:or_(l:append(Ts,Fs))).

%% any/2
any_badarg1_test() ->
    ?assertError(badarg, l:any(notfun, [])).
any_badarg2_test() ->
    ?assertError(badarg, l:any(fun(_) -> true end, notlist)).
any_empty_test() ->
    ?assertEqual(false, l:any(fun is_true/1, [])).
prop_any_always_true() ->
    ?FORALL({Fs,Ts}, {list(false), non_empty(list(true))},
            true == l:any(fun is_true/1, l:append(Fs,Ts))).
prop_any_always_false() ->
    ?FORALL(Fs, non_empty(list(false)),
            false == l:any(fun is_true/1, Fs)).

%% all/2
all_badarg1_test() ->
    ?assertError(badarg, l:all(notfun, [])).
all_badarg2_test() ->
    ?assertError(badarg, l:all(fun(_) -> true end, notlist)).
all_empty_test() ->
    ?assertEqual(true, l:all(fun is_true/1, [])).
prop_all_always_false() ->
    ?FORALL({Ts,Fs}, {list(true), non_empty(list(false))},
            false == l:all(fun is_true/1, l:append(Ts,Fs))).
prop_all_always_true() ->
    ?FORALL(Ts, list(true),
            true == l:all(fun is_true/1, Ts)).

%% sum/1
sum_badarg1_test() ->
    ?assertError(badarith, l:sum([a,b,c])).
sum_badarg2_test() ->
    ?assertError(badarg, l:sum(not_a_list)).
sum_null_list_test() ->
    ?assertEqual(0, l:sum([])).
prop_sum_singleton_equality() ->
    ?FORALL(X, integer(), l:sum([X]) == X).
prop_sum_recursive() ->
    ?FORALL([X|Xs], non_empty(list(integer())),
            X + l:sum(Xs) == l:sum([X|Xs])).

repF(N)-> fun(X)-> l:replicate(N,X) end.
is_true(X) -> X == true.
