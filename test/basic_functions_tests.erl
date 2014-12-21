-module(basic_functions_tests).
-include_lib("eunit/include/eunit.hrl").
-include_lib("triq/include/triq.hrl").

%%
%% Basic functions
%%

tq(Prop) -> ?_assert(triq:check(Prop,[],20)).

%% append/2
append_test_() ->
    [?_assertEqual([1,2], l:append([1], [2]))].

append_empty_left_test_() ->
    tq(?FORALL(Xs, list(any()), l:append([], Xs) == Xs)).

append_empty_right_test_() ->
    tq(?FORALL(Xs, list(any()), l:append(Xs, []) == Xs)).

append_inductive_left_test_() ->
    tq(?FORALL({Xs,Ys}, {non_empty(list(any())),non_empty(list(any()))},
            l:append(Xs, Ys) ==
                l:append([l:head(Xs)], l:append(l:tail(Xs), Ys)))).

append_inductive_right_test_() ->
    tq(?FORALL({Xs,Ys}, {non_empty(list(any())),non_empty(list(any()))},
               l:append(Xs, Ys) ==
                   l:append(Xs, l:append([l:head(Ys)], l:tail(Ys))))).

%% head/1
head_test_() ->
    [?_assertError(badarg, l:head([])),
     ?_assertEqual(1,      l:head([1]))
    ].
head_is_de_cons_test_() ->
    tq(?FORALL({X,Xs}, {any(), non_empty(list(any()))},
               l:head([X|Xs]) == X)).

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
tail_is_compliment_of_de_cons_test_() ->
    tq(?FORALL({X,Xs}, {any(), non_empty(list(any()))},
               l:tail([X|Xs]) == Xs)).

%% init/1
init_test_() ->
    [?_assertEqual([1,2],  l:init([1,2,3])),
     ?_assertError(badarg, l:init([]))
    ].

init_is_rev_tl_rev_test_() ->
    tq(?FORALL(Xs, non_empty(list(any())),
               l:init(Xs) == lists:reverse(tl(lists:reverse(Xs))))).

%% null/1
null_test_() ->
    [?_assertEqual(true, l:null([])),
     ?_assertEqual(true, l:null(""))
    ].
null_of_nonempty_is_false_test_() ->
    tq(?FORALL(Xs, non_empty(list(any())),
               l:null(Xs) == false)).

%% length/1
length_test_() ->
    [?_assertEqual(0, l:length([])),
     ?_assertEqual(1, l:length([a]))
    ].
length_of_nonempty_inductive_test_() ->
    tq(?FORALL(Xs, non_empty(list(any())),
               l:length(Xs) == 1 + l:length(tl(Xs)))).
