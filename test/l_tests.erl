-module(l_tests).

-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(PROP(A), ?_assert(proper:quickcheck(A()))).

%%
%% Basic functions
%%

%% append/2
append_two_singletons_test() ->
    ?assertMatch([1,2], l:append([1], [2])).

append_empty_left() ->
    ?FORALL(Xs, list(), l:append([], Xs) == Xs).

append_empty_right() ->
    ?FORALL(Xs, list(), l:append(Xs, []) == Xs).

append_is_plusplus() ->
    %% This is a cop-out. Todo: find inductive property
    ?FORALL({Xs,Ys}, {list(),list()},
            l:append(Xs, Ys) == Xs ++ Ys).


%% head/1
head_of_empty_list_test() ->
    ?assertError(badarg, l:head([])).

head_of_singleton_test() ->
    ?assertMatch(1, l:head([1])).

head_is_hd() ->
    %% Another cop-out.
    ?FORALL(Xs, non_empty(list()),
            l:head(Xs) == erlang:hd(Xs)).


%% last/1
last_of_empty_list_test() ->
    ?assertError(badarg, l:last([])).

last_of_singleton_test() ->
    ?assertMatch(1, l:last([1])).

last_of_list_test() ->
    ?assertMatch(3, l:last([1,2,3])).

%% tail/1
%% init/1
%% null/1
%% length/1

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
     ?PROP(append_is_plusplus),
     ?PROP(head_is_hd),
     ?PROP(reverse_inductive1)
    ].
