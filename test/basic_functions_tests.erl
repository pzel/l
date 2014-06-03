-module(basic_functions_tests).
-include_lib("proper_eunit/include/pt_proper_eunit.hrl").

%%
%% Basic functions
%%

%% append/2
prop_append_empty_left() ->
    ?FORALL(Xs, list(), l:append([], Xs) == Xs).
prop_append_empty_right() ->
    ?FORALL(Xs, list(), l:append(Xs, []) == Xs).
append_two_singletons_test() ->
    ?assertEqual([1,2], l:append([1], [2])).
prop_append_inductive_left() ->
    ?FORALL({Xs,Ys}, {non_empty(list()),non_empty(list())},
            l:append(Xs, Ys) ==
                l:append([l:head(Xs)], l:append(l:tail(Xs), Ys))).
prop_append_inductive_right() ->
    ?FORALL({Xs,Ys}, {non_empty(list()),non_empty(list())},
               l:append(Xs, Ys) ==
                   l:append(Xs, l:append([l:head(Ys)], l:tail(Ys)))).

%% head/1
head_of_empty_list_test() ->
    ?assertError(badarg, l:head([])).
head_of_singleton_test() ->
    ?assertEqual(1, l:head([1])).
prop_head_is_de_cons() ->
    ?FORALL({X,Xs}, {term(), non_empty(list())},
            l:head([X|Xs]) == X).

%% last/1
last_of_empty_list_test() ->
    ?assertError(badarg, l:last([])).
last_of_singleton_test() ->
    ?assertEqual(1, l:last([1])).
last_of_list_test() ->
    ?assertEqual(3, l:last([1,2,3])).

%% tail/1
tail_of_empty_list_test() ->
    ?assertError(badarg, l:tail([])).
tail_of_singleton_test() ->
    ?assertEqual([], l:tail([1])).
prop_tail_is_compliment_of_de_cons() ->
    ?FORALL({X,Xs}, {term(), non_empty(list())},
            l:tail([X|Xs]) == Xs).

%% init/1
init_of_empty_list_test() ->
    ?assertError(badarg, l:init([])).

prop_init_is_rev_tl_rev() ->
    ?FORALL(Xs, non_empty(list()),
            l:init(Xs) == lists:reverse(tl(lists:reverse(Xs)))).

%% null/1
null_of_empty_list_test() ->
    ?assertEqual(true, l:null([])).
prop_null_of_nonempty_is_false() ->
    ?FORALL(Xs, non_empty(list()),
            l:null(Xs) == false).

%% length/1
length_of_empty_list_test() ->
    ?assertEqual(0, l:length([])).
prop_length_of_nonempty_inductive() ->
    ?FORALL(Xs, non_empty(list()),
            l:length(Xs) == 1 + l:length(tl(Xs))).


