-module(predicates_tests).
-include_lib("eunit/include/eunit.hrl").
-include_lib("triq/include/triq.hrl").
-import(helpers, [tq/1]).

%% is_prefix_of/2

is_prefix_of_test_() ->
    [?_assertEqual(true,   l:is_prefix_of("", "")),
     ?_assertEqual(true,   l:is_prefix_of("","hello")),
     ?_assertEqual(true,   l:is_prefix_of("a","abc")),
     ?_assertEqual(false,  l:is_prefix_of("b","abc")),
     ?_assertError(badarg, l:is_prefix_of(x, "abc")),
     ?_assertError(badarg, l:is_prefix_of("a", 100))
    ].

all_inits_are_prefixes_test_() ->
    tq(?FORALL(L, list(any()),
               l:all(fun(Init) -> l:is_prefix_of(Init, L) end,
                     l:inits(L)))).

all_inits_are_prefixes_of_longer_list_test_() ->
    tq(?FORALL({L1, L2}, {list(int()), list(int())},
               l:all(fun(Init) -> l:is_prefix_of(Init, L1++L2) end,
                     l:inits(L1)))).

%% is_suffix_of/2

is_suffix_of_test_() ->
    [?_assertEqual(true,   l:is_suffix_of("", "")),
     ?_assertEqual(true,   l:is_suffix_of("", "abc")),
     ?_assertEqual(true,   l:is_suffix_of("c", "abc")),
     ?_assertEqual(true,   l:is_suffix_of("bc", "abc")),
     ?_assertError(badarg, l:is_suffix_of(y, "abc")),
     ?_assertError(badarg, l:is_suffix_of("a", 200))
    ].

all_tails_are_sufffixes_of_longer_list_test_() ->
    tq(?FORALL({L1, L2}, {list(int()), list(int())},
            l:all(fun(Tail) -> l:is_suffix_of(Tail, L1++L2) end,
                  l:tails(L2)))).

all_tails_are_suffixes_test_() ->
    tq(?FORALL(L, list(any()),
            l:all(fun(Tail) -> l:is_suffix_of(Tail, L) end,
                  l:tails(L)))).


%% is_infix_of/2
is_infix_of_test_() ->
    [?_assertEqual(true,   l:is_infix_of("", "")),
     ?_assertEqual(true,   l:is_infix_of("", "Erlang is OK")),
     ?_assertEqual(true,   l:is_infix_of("is", "Erlang is OK")),
     ?_assertEqual(false,  l:is_infix_of("isa", "Erlang is OK")),
     ?_assertError(badarg, l:is_infix_of(y, "abc")),
     ?_assertError(badarg, l:is_infix_of("a", 200))
    ].

infix_of_test_() ->
    tq(?FORALL({L1, L2, L3}, {list(int()), list(int()), list(int())},
               l:is_infix_of(L2, L1++L2++L3))).
