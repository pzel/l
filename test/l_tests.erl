-module(l_tests).

-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(PROP(A), ?_assert(proper:quickcheck(A()))).

%%
%% Basic functions
%%

%% append/2
append_empty_left() ->
    ?FORALL(Xs, list(), l:append([], Xs) == Xs).
append_empty_right() ->
    ?FORALL(Xs, list(), l:append(Xs, []) == Xs).
append_two_singletons_test() ->
    ?assertMatch([1,2], l:append([1], [2])).
append_inductive_left() ->
    ?FORALL({Xs,Ys}, {non_empty(list()),non_empty(list())},
            l:append(Xs, Ys) ==
                l:append([l:head(Xs)], l:append(l:tail(Xs), Ys))).
append_inductive_right() ->
    ?FORALL({Xs,Ys}, {non_empty(list()),non_empty(list())},
            l:append(Xs, Ys) ==
                l:append(Xs, l:append([l:head(Ys)], l:tail(Ys)))).

%% head/1
head_of_empty_list_test() ->
    ?assertError(badarg, l:head([])).
head_of_singleton_test() ->
    ?assertMatch(1, l:head([1])).
head_is_de_cons() ->
    ?FORALL({X,Xs}, {term(), non_empty(list())},
            l:head([X|Xs]) == X).

%% last/1
last_of_empty_list_test() ->
    ?assertError(badarg, l:last([])).
last_of_singleton_test() ->
    ?assertMatch(1, l:last([1])).
last_of_list_test() ->
    ?assertMatch(3, l:last([1,2,3])).

%% tail/1
tail_of_empty_list_test() ->
    ?assertError(badarg, l:tail([])).
tail_of_singleton_test() ->
    ?assertMatch([], l:tail([1])).
tail_is_compliment_of_de_cons() ->
    ?FORALL({X,Xs}, {term(), non_empty(list())},
            l:tail([X|Xs]) == Xs).


%% init/1
init_of_empty_list_test() ->
    ?assertError(badarg, l:init([])).

init_is_rev_tl_rev() ->
    ?FORALL(Xs, non_empty(list()),
            l:init(Xs) == lists:reverse(tl(lists:reverse(Xs)))).

%% null/1
null_of_empty_list_test() ->
    ?assertMatch(true, l:null([])).
null_of_nonempty_is_false() ->
    ?FORALL(Xs, non_empty(list()),
            l:null(Xs) == false).

%% length/1
length_of_empty_list_test() ->
    ?assertMatch(0, l:length([])).
length_of_nonempty_inductive() ->
    ?FORALL(Xs, non_empty(list()),
            l:length(Xs) == 1 + l:length(tl(Xs))).


%%
%% List transformations
%%

%% map/2
%% reverse/1
reverse_of_empty_test() ->
    ?assertMatch([], l:reverse([])).

reverse_of_singleton_test() ->
    ?assertMatch([1], l:reverse([1])).

reverse_inductive1() ->
    ?FORALL(Xs, non_empty(list()),
            l:reverse(tl(Xs)) ++ [hd(Xs)] == l:reverse(Xs)).

%%
%% Property instantiation
%%

basic_properties_test_() ->
    [?PROP(append_empty_left),
     ?PROP(append_empty_right),
     ?PROP(append_inductive_left),
     ?PROP(append_inductive_right),
     ?PROP(head_is_de_cons),
     ?PROP(tail_is_compliment_of_de_cons),
     ?PROP(reverse_inductive1),
     ?PROP(init_is_rev_tl_rev),
     ?PROP(null_of_nonempty_is_false),
     ?PROP(length_of_nonempty_inductive)
    ].
