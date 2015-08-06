-module(list_transformations_tests).
-include_lib("eunit/include/eunit.hrl").
-include_lib("triq/include/triq.hrl").
-import(helpers, [tq/1]).

%%
%% List transformations
%%

%% map/2
map_of_empty_test() ->
    ?assertEqual([], l:map(fun(X)-> X end, [])).
map_inductive_test_() ->
    F = fun(X) -> X+1 end,
    tq(?FORALL(Xs, non_empty(list(int())),
               l:map(F, Xs) == [F(hd(Xs)) | l:map(F,tl(Xs))])).

%% reverse/1
reverse_test_() ->
    [?_assertEqual([],  l:reverse([])),
     ?_assertEqual([1], l:reverse([1])),
     ?_assertError(badarg, l:reverse(notlist))
    ].
reverse_inductive1_test_() ->
    tq(?FORALL(Xs, non_empty(list(any())),
               l:reverse(tl(Xs)) ++ [hd(Xs)] == l:reverse(Xs))).
reverse_twice_is_id_test_() ->
    tq(?FORALL(Xs, non_empty(list(any())),
               l:reverse(l:reverse(Xs)) == Xs)).

%% intersperse/2
intersperse_test_() ->
    [?_assertEqual([],      l:intersperse(0,[])),
     ?_assertEqual([1],     l:intersperse(0,[1])),
     ?_assertEqual([1,0,2], l:intersperse(0,[1,2])),
     ?_assertError(badarg, l:intersperse(0,notlist))
    ].
intersperse_length_test_() ->
    tq(?FORALL(Xs, non_empty(list(any())),
               length(l:intersperse(hd(Xs),Xs)) == length(Xs) * 2 - 1)).

%% intercalate/2
intercalate_test_() ->
    [?_assertEqual([], l:intercalate([], [])),
     ?_assertEqual([7], l:intercalate([1], [[7]])),
     ?_assertEqual([7,1,8], l:intercalate([1], [[7],[8]])),
     ?_assertError(badarg, l:intercalate([1], [[7],[8], notlist])),
     ?_assertError(badarg, l:intercalate(notlist, [[7],[8]]))
    ].

%% transpose/1
transpose_test_() ->
    [?_assertEqual([],                  l:transpose([])),
     ?_assertEqual([],                  l:transpose([[]])),
     ?_assertEqual([[1]],               l:transpose([[1]])),
     ?_assertEqual([[1,4],[2,5],[3,6]], l:transpose([[1,2,3],[4,5,6]])),
     ?_assertError(badarg,              l:transpose([[1,2,3,99],[4,5,6]]))
    ].

transpose_twice_is_id_test_() ->
    tq(?FORALL(Xs, helpers:matrix(int()),
               l:transpose(l:transpose(Xs)) == Xs)).

length_transpose_is_length_head_test_() ->
    tq(?FORALL(Xs, helpers:matrix(int()),
               l:length(l:transpose(Xs)) == length(hd(Xs)))).

% subsequences/1
subsequences_test_() ->
    [?_assertEqual([""],      l:subsequences("")),
     ?_assertEqual(["", "a"], l:subsequences("a")),
     ?_assertEqual ([[],"c","b","bc","a","ac","ab","abc"],
                    l:subsequences("abc")),
     ?_assertError(badarg, l:subsequences(notlist))
    ].
subsequences_length_test_() ->
    tq(?FORALL(Xs, helpers:short_list(int()),
               l:length(l:subsequences(Xs)) == math:pow(2,length(Xs)))).

% permutations/1
permutations_empty_test_() ->
    [?_assertEqual([""],  l:permutations("")),
     ?_assertEqual(["a"], l:permutations("a")),
     ?_assertEqual(["abc","acb","bac","bca","cab","cba"],
                 l:permutations("abc")),
     ?_assertError(badarg, l:permutations(notlist))
     ].

%% This property times out. Research how to increase eunit timeout.
%% prop_permutations_length() ->
%%     tq(?FORALL(Xs, helpers:short_list(char()),
%%             l:length(l:permutations(Xs)) ==
%%                 helpers:factorial(length(Xs))).

%% Instead of quickchecking, we'll just make the lists ourselves
permutations_manual_qc_test_() ->
    Fac = fun helpers:factorial/1,
    LP = fun(X) -> l:length(l:permutations(X)) end,
    [?_assertEqual(Fac(0), LP("")),
     ?_assertEqual(Fac(1), LP("a")),
     ?_assertEqual(Fac(2), LP("ab")),
     ?_assertEqual(Fac(2), LP("aa")),
     ?_assertEqual(Fac(3), LP("abc")),
     ?_assertEqual(Fac(3), LP("aaa")),
     ?_assertEqual(Fac(4), LP("abcd")),
     ?_assertEqual(Fac(4), LP("aaaa")),
     ?_assertEqual(Fac(5), LP("abcde")),
     ?_assertEqual(Fac(5), LP("aaaaa"))
    ].
