-module(indexing_lists_tests).
-include_lib("eunit/include/eunit.hrl").
-include_lib("triq/include/triq.hrl").
-include_lib("helpers.hrl").
-import(helpers, [const/1, id/0]).

'!!_test_'() ->
    [?_assertEqual(a,  l:'!!'([a,b], 0))].

index_test_() ->
    [?_assertEqual(a, l:index([a,b], 0)),
     ?_assertEqual(b, l:index([a,b], 1)),
     ?_assertError(badarg, l:index(notlist, 0)),
     ?_assertError(index_too_large, l:index([a,b], 2)),
     ?_assertError(negative_index, l:index([a,b], -1)),
     ?forall(L, non_empty(list(int())),
                l:index(L, length(L) - 1) == l:last(L))
    ].

elem_index_test_() ->
    [?_assertEqual({0}, l:elem_index(a, [a,b])),
     ?_assertEqual({1}, l:elem_index(b, [a,b])),
     ?_assertEqual({}, l:elem_index(c, [a,b])),
     ?_assertError(badarg, l:elem_index(a, little_lamb)),
     ?forall(L, non_empty(list(int())),
                {0} == l:elem_index(hd(L), L))
    ].

elem_indices_test_() ->
    [?_assertEqual([], l:elem_indices(a, [])),
     ?_assertEqual([0], l:elem_indices(a, [a,b,c])),
     ?_assertEqual([1,3,5], l:elem_indices($a, "banana")),
     ?_assertError(badarg, l:elem_indices(elem, entary)),
     ?forall({L1,L2,L3},
                {list(pos_integer()), list(pos_integer()), list(pos_integer())},
                [length(L1),
                 length(L1) + 1 + length(L2),
                 length(L1) + 1 + length(L2) + 1 + length(L3)]
                == l:elem_indices(0, L1 ++ [0] ++ L2 ++ [0] ++ L3 ++ [0]))
    ].

find_index_test_() ->
    [?_assertEqual({}, l:find_index(const(true), [])),
     ?_assertEqual({}, l:find_index(const(false), [])),
     ?_assertEqual({0}, l:find_index(id(), [true])),
     ?_assertEqual({1}, l:find_index(id(), [false,true])),
     ?_assertError(badarg, l:find_index(foo, [])),
     ?_assertError(badarg, l:find_index(id(), not_list)),
     ?forall({L, N},
             {list(pos_integer()), pos_integer()},
             {0} == l:find_index(lt(0), [neg(N)|L])),
     ?forall({L, N},
             {list(pos_integer()), pos_integer()},
             {length(L)} ==
                 l:find_index(lt(0), l:reverse([neg(N)|L])))
    ].


lt(X) -> fun(Y) -> Y < X end.
neg(X) -> X * -1.
