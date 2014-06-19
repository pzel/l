-module(special_folds_tests).
-include_lib("proper_eunit/include/pt_proper_eunit.hrl").

%%
%%  Special folds
%%

%% concat/1
concat_empty_test() ->
    ?assertEqual([], l:concat("")).
concat_one_test() ->
    ?assertEqual("a", l:concat(["a"])).
concat_two_test() ->
    ?assertEqual("ab", l:concat(["a", "b"])).
concat_three_test() ->
    ?assertEqual("abc", l:concat(["a", "b", "c"])).

%% concat_map/2
concat_map_empty_test() ->
    ?assertEqual([], l:concat_map(fun wrap/1, [])).
prop_concat_map_id() ->
    ?FORALL(Xs, list(),
            Xs == l:concat_map(fun wrap/1, Xs)).
prop_concat_map_length() ->
    ?FORALL(Xs, list(integer()),
            l:length(Xs) * 2 == length(l:concat_map(fun wrap_dup/1, Xs))).

%% Helpers
wrap(X) -> [X].

%% TODO: rewrite the concat_map_length property by generating a function
%%       F that duplicates its argument N times and verify that
%%       length(Xs) * N
%%       ==
%%       length(l:concat_map(make_wrap(N), Xs)
%%       l:replicate is needed to do this
wrap_dup(X) -> [X,X].
