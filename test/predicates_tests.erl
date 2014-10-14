-module(predicates_tests).
-include_lib("proper_eunit/include/pt_proper_eunit.hrl").

%% is_prefix_of/2

is_prefix_of_test_() ->
    [?_assertEqual(true,   l:is_prefix_of("", "")),
     ?_assertEqual(true,   l:is_prefix_of("","hello")),
     ?_assertEqual(true,   l:is_prefix_of("a","abc")),
     ?_assertEqual(false,  l:is_prefix_of("b","abc")),
     ?_assertError(badarg, l:is_prefix_of(x, "abc")),
     ?_assertError(badarg, l:is_prefix_of("a", 100))
    ].

prop_is_prefix_of_appended() ->
    ?FORALL({L1, L2}, {list(integer()), list(integer())},
            l:is_prefix_of(L1, L1++L2)).
