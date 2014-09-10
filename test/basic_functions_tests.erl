-module(basic_functions_tests).
-include_lib("proper_eunit/include/pt_proper_eunit.hrl").

%%
%% Basic functions
%%

%% append/2
append_test_() ->
    [?_assertEqual([1,2], l:append([1], [2]))].

prop_append_empty_left() ->
    ?FORALL(Xs, list(), l:append([], Xs) == Xs).
prop_append_empty_right() ->
    ?FORALL(Xs, list(), l:append(Xs, []) == Xs).
prop_append_inductive_left() ->
    ?FORALL({Xs,Ys}, {non_empty(list()),non_empty(list())},
            l:append(Xs, Ys) ==
                l:append([l:head(Xs)], l:append(l:tail(Xs), Ys))).
prop_append_inductive_right() ->
    ?FORALL({Xs,Ys}, {non_empty(list()),non_empty(list())},
               l:append(Xs, Ys) ==
                   l:append(Xs, l:append([l:head(Ys)], l:tail(Ys)))).

%% head/1
head_test_() ->
    [?_assertError(badarg, l:head([])),
     ?_assertEqual(1,      l:head([1]))
    ].
prop_head_is_de_cons() ->
    ?FORALL({X,Xs}, {term(), non_empty(list())},
            l:head([X|Xs]) == X).

%% last/1
last_test_() ->
    [?_assertError(badarg, l:last([])),
     ?_assertEqual(1,      l:last([1])),
     ?_assertEqual(3,      l:last([1,2,3]))
    ].

%% tail/1
tail_test_() ->
    [?_assertError(badarg, l:tail([])),
     ?_assertEqual([], l:tail([1]))
    ].
prop_tail_is_compliment_of_de_cons() ->
    ?FORALL({X,Xs}, {term(), non_empty(list())},
            l:tail([X|Xs]) == Xs).

%% init/1
init_test_() ->
    [?_assertEqual([1,2],  l:init([1,2,3])),
     ?_assertError(badarg, l:init([]))
    ].

prop_init_is_rev_tl_rev() ->
    ?FORALL(Xs, non_empty(list()),
            l:init(Xs) == lists:reverse(tl(lists:reverse(Xs)))).

%% null/1
null_test_() ->
    [?_assertEqual(true, l:null([])),
     ?_assertEqual(true, l:null(""))
    ].
prop_null_of_nonempty_is_false() ->
    ?FORALL(Xs, non_empty(list()),
            l:null(Xs) == false).

%% length/1
length_test_() ->
    [?_assertEqual(0, l:length([])),
     ?_assertEqual(1, l:length([a]))
    ].
prop_length_of_nonempty_inductive() ->
    ?FORALL(Xs, non_empty(list()),
            l:length(Xs) == 1 + l:length(tl(Xs))).
