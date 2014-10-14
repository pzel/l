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

prop_all_inits_are_prefixes() ->
    ?FORALL(L, list(),
            l:all(fun(Init) -> l:is_prefix_of(Init, L) end,
                  l:inits(L))).

prop_all_inits_are_prefixes_of_longer_list() ->
    ?FORALL({L1, L2}, {list(integer()), list(integer())},
            l:all(fun(Init) -> l:is_prefix_of(Init, L1++L2) end,
                  l:inits(L1))).

%% is_suffix_of/2

is_suffix_of_test_() ->
    [?_assertEqual(true,   l:is_suffix_of("", "")),
     ?_assertEqual(true,   l:is_suffix_of("", "abc")),
     ?_assertEqual(true,   l:is_suffix_of("c", "abc")),
     ?_assertEqual(true,   l:is_suffix_of("bc", "abc")),
     ?_assertError(badarg, l:is_suffix_of(y, "abc")),
     ?_assertError(badarg, l:is_suffix_of("a", 200))
    ].

prop_all_tails_are_sufffixes_of_longer_list() ->
    ?FORALL({L1, L2}, {list(integer()), list(integer())},
            l:all(fun(Tail) -> l:is_suffix_of(Tail, L1++L2) end,
                  l:tails(L2))).

prop_all_tails_are_suffixes() ->
    ?FORALL(L, list(),
            l:all(fun(Tail) -> l:is_suffix_of(Tail, L) end,
                  l:tails(L))).

