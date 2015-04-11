-module(zip_unzip_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("triq/include/triq.hrl").
-include_lib("helpers.hrl").

zip_test_() ->
    [ ?_assertEqual([], l:zip([], [])),
      ?_assertEqual([{1,2}], l:zip([1], [2])),
      ?_assertEqual([{1,2}], l:zip([1], [2,3])),
      ?_assertEqual([{1,2}], l:zip([1,3], [2])),
      ?_assertEqual([{1,$a}, {2,$b}, {3,$c}],
                    l:zip([1,2,3], "abc")),
      ?_assertError(badarg, l:zip(foo, [])),
      ?_assertError(badarg, l:zip([], bar))
    ].

zip_with_test_() ->
    [ ?_assertEqual([3,5,7], l:zip_with(fun add/2, [1,2,3], [2,3,4])),
      ?_assertError(badarg, l:zip_with(fun add/2, notlist, [])),
      ?_assertError(badarg, l:zip_with(fun add/2, [], wrong)),
      ?_assertError(badarg, l:zip_with(fun(X) -> X end, [], []))
    ].

add(A,B) -> A + B.
