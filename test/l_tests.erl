-module(l_tests).

-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(PROP(A), ?_assert(proper:quickcheck(A()))).

%% Basics

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


%%
%% Property instantiation
%%

basic_properties_test_() ->
    [?PROP(append_empty_left),
     ?PROP(append_empty_right),
     ?PROP(append_is_plusplus)
    ].
