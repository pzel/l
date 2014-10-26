-module(searching_lists_tests).
-include_lib("proper_eunit/include/pt_proper_eunit.hrl").

%%
%% Searching by equality
%%

%% elem/2
elem_test_() ->
    [?_assertEqual(true,  l:elem(1,[1])),
     ?_assertEqual(true,  l:elem(3,[1,2,3])),
     ?_assertEqual(false, l:elem(1,[])),
     ?_assertError(badarg, l:elem(1,notlist))
    ].

prop_elem() ->
    ?FORALL({L1, L2, El}, {list(integer()), list(integer()), integer()},
            l:elem(El, L1 ++ [El] ++ L2)).


%% not_elem/2
not_elem_test_() ->
    [?_assertEqual(false,  l:not_elem(1,[1])),
     ?_assertEqual(false,  l:not_elem(3,[1,2,3])),
     ?_assertEqual(true,   l:not_elem(1,[])),
     ?_assertError(badarg, l:not_elem(1,notlist))
    ].

%% lookup/2
lookup_test_() ->
    [?_assertEqual(nothing, l:lookup(a, [])),
     ?_assertEqual({just, b}, l:lookup(a, [{a,b}])),
     ?_assertError(badarg, l:lookup(a,fun()->foo end))
    ].

prop_lookup_failure() ->
    ?FORALL({Xs, NegK},
            {list({pos_integer(), term()}), neg_integer()},
            nothing == l:lookup(NegK, Xs)).
prop_lookup_success() ->
    ?FORALL({Xs, NegK, Val},
            {list({pos_integer(), term()}), neg_integer(), term()},
            {just, Val} == l:lookup(NegK, Xs ++ [{NegK, Val}])).



%%
%%  Searching with a predicate
%%


%% filter/2
filter_test_() ->
    [?_assertEqual([], l:filter(fun t/1, [])),
     ?_assertEqual([], l:filter(fun f/1, []))
    ].

prop_const_true_is_id() ->
    ?FORALL(Xs, list(integer()),
            l:filter(fun t/1, Xs) == Xs).
prop_const_false_is_null() ->
    ?FORALL(Xs, list(integer()),
            l:filter(fun f/1, Xs) == []).

prop_filter_half() ->
    ?FORALL({Negs, Poss}, {list(neg_integer()), list(pos_integer())},
            begin
                All = l:append(Negs,Poss),
                l:filter(fun pos/1, All) == Poss
                andalso
                l:filter(fun(X) -> not pos(X) end, All) == Negs
            end).

t(_) -> true.
f(_) -> false.
pos(N) -> N > 0.
