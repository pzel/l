-module(sublists_tests).
-include_lib("proper_eunit/include/pt_proper_eunit.hrl").

%% take/2
%%

take_test_() ->
    [?_assertEqual("Hello", l:take(5, "Hello, World!")),
     ?_assertEqual([1,2,3], l:take(3, [1,2,3,4,5])),
     ?_assertEqual([1,2],   l:take(3, [1,2])),
     ?_assertEqual([],      l:take(3, [])),
     ?_assertEqual([],      l:take(-1, [])),
     ?_assertEqual([],      l:take(0, [1,3])),
     ?_assertError(badarg,  l:take(foo, [1,2,3])),
     ?_assertError(badarg,  l:take(3, celestial_birds))
    ].

prop_take_identity() ->
    ?FORALL(L, list(),
            ?FORALL(N, choose(length(L), inf),
                l:take(N, L) == L)).

prop_take_as_reverse_drop() ->
    ?FORALL(L, non_empty(list()),
            ?FORALL(N, choose(0, length(L)),
                    l:take(N,L) ==
                        l:reverse(l:drop(length(L) - N,
                                         l:reverse(L))))).

%% drop/2
drop_test_() ->
    [?_assertEqual("World!", l:drop(6, "Hello World!")),
     ?_assertEqual([],       l:drop(3, [1,2])),
     ?_assertEqual([],       l:drop(3, [])),
     ?_assertEqual([1,2],    l:drop(-1, [1,2])),
     ?_assertError(badarg,   l:drop(foo, [1,2,3])),
     ?_assertError(badarg,   l:drop(7, diamonds))
    ].



%% split_at/2
split_at_test_() ->
    [?_assertEqual({"Hello ","World!"}, l:split_at(6,"Hello World!")),
     ?_assertEqual({[1,2,3],[4,5]},     l:split_at(3, [1,2,3,4,5])),
     ?_assertEqual({[1],[2,3]},         l:split_at(1, [1,2,3])),
     ?_assertEqual({[1,2,3],[]},        l:split_at(3, [1,2,3])),
     ?_assertEqual({[1,2,3],[]},        l:split_at(4, [1,2,3])),
     ?_assertEqual({[],[1,2,3]},        l:split_at(0, [1,2,3])),
     ?_assertEqual({[],[1,2,3]},        l:split_at(-1, [1,2,3])),
     ?_assertError(badarg,              l:split_at(goo, [1,2,3])),
     ?_assertError(badarg,              l:split_at(1, partridge_in_a_pear_tree))
    ].

prop_split_at_is_take_and_drop() ->
    ?FORALL(L, non_empty(list()),
            ?FORALL(N, choose(0, length(L)),
                    l:split_at(N, L) == {l:take(N,L), l:drop(N,L)})).

%% take_while/2
take_while_test_() ->
    Lt = fun(N)-> fun(X)-> X < N end end,
    [?_assertEqual([1,2],   l:take_while(Lt(3), [1,2,3,4,1,2,3,4])),
     ?_assertEqual([1,2,3], l:take_while(Lt(9), [1,2,3])),
     ?_assertEqual([],      l:take_while(Lt(0), [1,2,3])),
     ?_assertError(badarg,  l:take_while(cello, [1,2,3])),
     ?_assertError(badarg,  l:take_while(Lt(3), cliffs_of_dover))
    ].
