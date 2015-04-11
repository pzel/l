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

zip3_to_zipX_test_() ->
    %% todo: generate me,
    [ ?_assertEqual([{1,2,3}], l:zip3([1], [2], [3])),
      ?_assertEqual([], l:zip3([], [2], [3])),
      ?_assertEqual([], l:zip3([1], [], [3])),
      ?_assertEqual([], l:zip3([1], [2], [])),
      ?_assertError(badarg, l:zip3(hello, [2], [3])),
      ?_assertEqual([3,6,9], l:zip_with3(fun add/3, [1,2,3], [1,2,3], [1,2,3])),
      ?_assertError(badarg, l:zip_with3(fun add/2, [1,2,3], [1,2,3], [1,2,3]))
    ].


add(A,B) -> A + B.
add(A,B,C) -> A + B + C.
