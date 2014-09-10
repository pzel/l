-module(special_folds_tests).
-include_lib("proper_eunit/include/pt_proper_eunit.hrl").

%%
%%  Special folds
%%

%% concat/1
concat_test_() ->
    [?_assertEqual([],    l:concat("")),
     ?_assertEqual("a",   l:concat(["a"])),
     ?_assertEqual("ab",  l:concat(["a", "b"])),
     ?_assertEqual("abc", l:concat(["a", "b", "c"]))
    ].
prop_concat_lengths() ->
    ?FORALL(Xs, list(non_empty(list(integer()))),
            l:length(l:concat(Xs)) == l:sum(l:map(fun l:length/1, Xs))).

%% concat_map/2
concat_map_test_() ->
    [?_assertEqual([],    l:concat_map(repF(1), [])),
     ?_assertEqual([a],   l:concat_map(repF(1), [a])),
     ?_assertEqual([a,a], l:concat_map(repF(2), [a]))
    ].
prop_concat_map_id() ->
    ?FORALL(Xs, list(),
            Xs == l:concat_map(repF(1), Xs)).
prop_concat_map_length() ->
    ?FORALL({N,Xs}, {non_neg_integer(), list(integer())},
            l:length(Xs) * N == length(l:concat_map(repF(N), Xs))).

%% and/1
and_test_() ->
    [?_assertError(badarg, l:'and'([hello, 1, {}])),
     ?_assertEqual(true,   l:'and'([])),
     ?_assertEqual(true,   l:'and'([true])),
     ?_assertEqual(false,  l:'and'([true,false]))
    ].
prop_and_all_true() ->
    ?FORALL(Xs, list(true), true == l:'and'(Xs)).
prop_and_at_least_one_false() ->
    ?FORALL({Ts,Fs}, {list(true), non_empty(list(false))},
            false == l:'and'(l:append(Ts,Fs))).

%% or/1
or_test_() ->
    [?_assertError(badarg, l:'or'([hello, 1, {}])),
     ?_assertEqual(false,  l:'or'([])),
     ?_assertEqual(false,  l:'or'([false])),
     ?_assertEqual(true,   l:'or'([false,true]))
    ].
prop_or_all_false() ->
    ?FORALL(Xs, list(false), false == l:'or'(Xs)).
prop_or_at_least_one_true() ->
    ?FORALL({Fs,Ts}, {list(false), non_empty(list(true))},
            true == l:'or'(l:append(Ts,Fs))).

%% any/2
any_test_() ->
    [?_assertError(badarg, l:any(notfun, [])),
     ?_assertError(badarg, l:any(fun(_) -> true end, notlist)),
     ?_assertEqual(false,  l:any(fun is_true/1, [])),
     ?_assertEqual(false,  l:any(fun is_true/1, [false])),
     ?_assertEqual(true,   l:any(fun is_true/1, [false,true]))
    ].
prop_any_always_true() ->
    ?FORALL({Fs,Ts}, {list(false), non_empty(list(true))},
            true == l:any(fun is_true/1, l:append(Fs,Ts))).
prop_any_always_false() ->
    ?FORALL(Fs, non_empty(list(false)),
            false == l:any(fun is_true/1, Fs)).

%% all/2
all_test_() ->
    [?_assertError(badarg, l:all(notfun, [])),
     ?_assertError(badarg, l:all(fun(_) -> true end, notlist)),
     ?_assertEqual(true,   l:all(fun is_true/1, [])),
     ?_assertEqual(true,   l:all(fun is_true/1, [true])),
     ?_assertEqual(false,  l:all(fun is_true/1, [true,false]))
    ].
prop_all_always_false() ->
    ?FORALL({Ts,Fs}, {list(true), non_empty(list(false))},
            false == l:all(fun is_true/1, l:append(Ts,Fs))).
prop_all_always_true() ->
    ?FORALL(Ts, list(true),
            true == l:all(fun is_true/1, Ts)).

%% sum/1
sum_test_() ->
    [?_assertError(badarith, l:sum([we,arent,numbers])),
     ?_assertError(badarg,   l:sum(not_a_list)),
     ?_assertEqual(0,        l:sum([]))
    ].
prop_sum_singleton_equality() ->
    ?FORALL(X, integer(), l:sum([X]) == X).
prop_sum_recursive() ->
    ?FORALL([X|Xs], non_empty(list(integer())),
            X + l:sum(Xs) == l:sum([X|Xs])).

%% product/1
product_test_() ->
    [?_assertError(badarith, l:product([a,b,c])),
     ?_assertError(badarg,   l:product(not_a_list)),
     ?_assertEqual(1,        l:product([]))
     ].
prop_product_singleton_equality() ->
    ?FORALL(X, integer(), l:product([X]) == X).
prop_product_recursive() ->
    ?FORALL([X|Xs], non_empty(list(integer())),
            X * l:product(Xs) == l:product([X|Xs])).

%% maximum/1
maximum_test_() ->
    [?_assertError(badarg, l:maximum(not_a_list)),
     ?_assertError(badarg, l:maximum([])),
     ?_assertEqual(1,      l:maximum([1])),
     ?_assertEqual(2,      l:maximum([1,2])),
     ?_assertEqual(0,      l:maximum([0,-1]))
    ].
prop_maximum_singleton_equality() ->
    ?FORALL(X, integer(),
            X == l:maximum([X])).
prop_maximum_is_biggest_int() ->
    ?FORALL(X, integer(-1000,1000),
            ?FORALL(Xs, list(integer(-1000,X)),
                    X == l:maximum(Xs ++ [X]))).
%% minimum/1
minimum_test_() ->
    [?_assertError(badarg, l:minimum(not_a_list)),
     ?_assertError(badarg, l:minimum([])),
     ?_assertEqual(1,      l:minimum([1])),
     ?_assertEqual(1,      l:minimum([1,2])),
     ?_assertEqual(-1,     l:minimum([0,-1]))
    ].
prop_minimum_singleton_equality() ->
    ?FORALL(X, integer(),
            X == l:minimum([X])).
prop_minimum_is_smallest_int() ->
    ?FORALL(X, integer(-1000,1000),
            ?FORALL(Xs, list(integer(X,1000)),
                    X == l:minimum(Xs ++ [X]))).

repF(N)-> fun(X)-> l:replicate(N,X) end.
is_true(X) -> X == true.
