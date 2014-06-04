-module(list_transformations_tests).
-include_lib("proper_eunit/include/pt_proper_eunit.hrl").

%%
%% List transformations
%%

%% map/2
map_of_empty_test() ->
    ?assertEqual([], l:map(fun(X)-> X end, [])).
prop_map_inductive() ->
    F = fun(X) -> X+1 end,
    ?FORALL(Xs, non_empty(list(integer())),
            l:map(F, Xs) == [F(hd(Xs)) | l:map(F,tl(Xs))]).

%% reverse/1
reverse_of_empty_test() ->
    ?assertEqual([], l:reverse([])).
reverse_of_singleton_test() ->
    ?assertEqual([1], l:reverse([1])).
prop_reverse_inductive1() ->
    ?FORALL(Xs, non_empty(list()),
            l:reverse(tl(Xs)) ++ [hd(Xs)] == l:reverse(Xs)).
prop_reverse_twice_is_id() ->
    ?FORALL(Xs, non_empty(list()),
            l:reverse(l:reverse(Xs)) == Xs).


%% intersperse/2
intersperse_empty_test() ->
    ?assertEqual([], l:intersperse(0,[])).
intersperse_one_test() ->
    ?assertEqual([1], l:intersperse(0,[1])).
intersperse_two_test() ->
    ?assertEqual([1,0,2], l:intersperse(0,[1,2])).
prop_intersperse_length() ->
    ?FORALL(Xs, non_empty(list()),
            length(l:intersperse(hd(Xs),Xs)) == length(Xs) * 2 - 1).

%% intercalate/2
intercalate_empty_test() ->
    ?assertEqual([], l:intercalate([], [])).
intercalate_one_test() ->
    ?assertEqual([7], l:intercalate([1], [[7]])).
intercalate_two_test() ->
    ?assertEqual([7,1,8], l:intercalate([1], [[7],[8]])).

%% transpose/1
transpose_empty_test() ->
    ?assertEqual([], l:transpose([])).
transpose_list_of_empty_test() ->
    ?assertEqual([], l:transpose([[]])).
transpose_one_test() ->
    ?assertEqual([[1]], l:transpose([[1]])).
transpose_two_test() ->
    ?assertEqual([[1,4],[2,5],[3,6]],
                 l:transpose([[1,2,3],[4,5,6]])).
transpose_diff_length_fail_test() ->
    ?assertError(badarg,
                 l:transpose([[1,2,3,99],[4,5,6]])).
prop_transpose_twice_is_id() ->
    ?FORALL(Xs, helpers:matrix(integer()),
            l:transpose(l:transpose(Xs)) == Xs).
prop_length_transpose_is_length_head() ->
    ?FORALL(Xs, helpers:matrix(integer()),
            l:length(l:transpose(Xs)) == length(hd(Xs))).

% subsequences/1
subsequences_empty_test() ->
    ?assertEqual([""], l:subsequences("")).
subsequences_one_test() ->
    ?assertEqual(["", "a"], l:subsequences("a")).
subsequences_abc_test() ->
    ?assertEqual([[],"c","b","bc","a","ac","ab","abc"],
                 l:subsequences("abc")).
prop_subsequences_length() ->
    ?FORALL(Xs, helpers:short_list(integer()),
            l:length(l:subsequences(Xs)) ==
                erlang:trunc(math:pow(2,length(Xs)))).

% permutations/1
permutations_empty_test() ->
    ?assertEqual([""], l:permutations("")).
permutations_singleton_test() ->
    ?assertEqual(["a"], l:permutations("a")).
permutations_abc_test() ->
    ?assertEqual(["abc","acb","bac","bca","cab","cba"],
                 l:permutations("abc")).


%% This property times out. Research how to increase eunit timeout.
%% prop_permutations_length() ->
%%     ?FORALL(Xs, helpers:short_list(char()),
%%             l:length(l:permutations(Xs)) ==
%%                 helpers:factorial(length(Xs))).

%% Instead of quickchecking, we'll just make the lists ourselves
permutations_manual_test() ->
    Fac = fun helpers:factorial/1,
    LP = fun(X) -> l:length(l:permutations(X)) end,
    ?assertEqual(Fac(0), LP("")),
    ?assertEqual(Fac(1), LP("a")),
    ?assertEqual(Fac(2), LP("ab")),
    ?assertEqual(Fac(2), LP("aa")),
    ?assertEqual(Fac(3), LP("abc")),
    ?assertEqual(Fac(3), LP("aaa")),
    ?assertEqual(Fac(4), LP("abcd")),
    ?assertEqual(Fac(4), LP("aaaa")),
    ?assertEqual(Fac(5), LP("abcde")),
    ?assertEqual(Fac(5), LP("aaaaa")).
