-module(indexing_lists_tests).
-include_lib("eunit/include/eunit.hrl").
-include_lib("triq/include/triq.hrl").
-import(helpers, [tq/1]).

'!!_test_'() ->
    [?_assertEqual(a,  l:'!!'([a,b], 0))].

index_test_() ->
    [?_assertEqual(a, l:index([a,b], 0)),
     ?_assertEqual(b, l:index([a,b], 1)),
     ?_assertError(badarg, l:index(notlist, 0)),
     ?_assertError(index_too_large, l:index([a,b], 2)),
     ?_assertError(negative_index, l:index([a,b], -1)),
     tq(?FORALL(L, non_empty(list(int())),
                l:index(L, length(L) - 1) == l:last(L)))
    ].
