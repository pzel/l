-module(searching_lists_tests).
-include_lib("eunit/include/eunit.hrl").
-include_lib("triq/include/triq.hrl").
-import(helpers, [tq/1]).

neg_integer() -> dom_fun:neg_integer().

%%
%% Searching by equality
%%

%% elem/2
elem_test_() ->
    [?_assertEqual(true,  l:elem(1,[1])),
     ?_assertEqual(true,  l:elem(3,[1,2,3])),
     ?_assertEqual(false, l:elem(1,[])),
     ?_assertError(badarg, l:elem(1,notlist)),

     tq(?FORALL({L1, L2, El}, {list(int()), list(int()), int()},
                l:elem(El, L1 ++ [El] ++ L2)))
    ].


%% not_elem/2
not_elem_test_() ->
    [?_assertEqual(false,  l:not_elem(1,[1])),
     ?_assertEqual(false,  l:not_elem(3,[1,2,3])),
     ?_assertEqual(true,   l:not_elem(1,[])),
     ?_assertError(badarg, l:not_elem(1,notlist))
    ].

%% lookup/2
lookup_test_() ->
    [?_assertEqual({}, l:lookup(a, [])),
     ?_assertEqual({b}, l:lookup(a, [{a,b}])),
     ?_assertError(badarg, l:lookup(a,fun()->foo end)),

     tq(?FORALL({Xs, NegK},
                {list({pos_integer(), any()}), neg_integer()},
                {} == l:lookup(NegK, Xs))),

     tq(?FORALL({Xs, NegK, Val},
                {list({pos_integer(), any()}), neg_integer(), any()},
                {Val} == l:lookup(NegK, Xs ++ [{NegK, Val}])))
    ].

%%
%%  Searching with a predicate
%%

%% find/2
find_test_() ->
    [?_assertEqual({}, l:find(eq(a), [])),
     ?_assertEqual({a}, l:find(eq(a), [a])),
     ?_assertError(badarg, l:find(notfun, [x])),
     ?_assertError(badarg, l:find(eq(1), notlist)),

     tq(?FORALL({Xs, NegK}, {list(pos_integer()), neg_integer()},
                {} == l:find(eq(NegK), Xs))),

     tq(?FORALL({Xs, NegK}, {list(pos_integer()), neg_integer()},
                {NegK} == l:find(eq(NegK), Xs++[NegK])))
    ].

%% filter/2
filter_test_() ->
    [?_assertEqual([], l:filter(fun t/1, [])),
     ?_assertEqual([], l:filter(fun f/1, [])),
     ?_assertError(badarg, l:filter(notfun, [])),
     ?_assertError(badarg, l:filter(fun t/1, notlist)),

     tq(?FORALL(Xs, list(int()),
                l:filter(fun t/1, Xs) == Xs)),
     tq(?FORALL(Xs, list(int()),
                l:filter(fun f/1, Xs) == [])),
     tq(?FORALL({Negs, Poss}, {list(neg_integer()), list(pos_integer())},
                begin
                    All = l:append(Negs,Poss),
                    l:filter(fun pos/1, All) == Poss
                        andalso
                        l:filter(fun(X) -> not pos(X) end, All) == Negs
                end))
    ].


%% partition/2
partition_test_() ->
    [?_assertEqual({[], []},   l:partition(eq(0), [])),
     ?_assertEqual({[0], [1]},   l:partition(eq(0), [0,1])),
     ?_assertError(badarg, l:partition(notfun, [])),
     ?_assertError(badarg, l:partition(eq(0), notlist)),

     tq(?FORALL({X, Xs}, {int(), list(int())},
                {l:filter(lt(X), Xs), l:filter(not_lt(X), Xs)}
                == l:partition(lt(X), Xs)))
    ].





t(_) -> true.
f(_) -> false.
pos(N) -> N > 0.
lt(X) -> fun(Y) -> Y < X end.
not_lt(X) -> fun(Y) -> not (Y < X) end.
eq(X) -> fun(Y) -> Y == X end.
