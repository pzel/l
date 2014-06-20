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

%% @TODO:
%% Add the property:
%% forAll Xs = length(concat(Xs) == sum(map length Xs)
%% we need the sum function for this

%% concat_map/2
concat_map_empty_test() ->
    ?assertEqual([], l:concat_map(repF(1), [])).
prop_concat_map_id() ->
    ?FORALL(Xs, list(),
            Xs == l:concat_map(repF(1), Xs)).
prop_concat_map_length() ->
    ?FORALL({N,Xs}, {non_neg_integer(), list(integer())},
            l:length(Xs) * N == length(l:concat_map(repF(N), Xs))).

%% and/1
and_badarg_test() ->
    ?assertError(badarg, l:and_([hello, 1, {}])).
and_empty_test() ->
    ?assertEqual(true, l:and_([])).
prop_and_all_true() ->
    ?FORALL(Xs, list(true), true == l:and_(Xs)).
prop_and_at_least_one_false() ->
    ?FORALL({Ts,Fs}, {list(true), non_empty(list(false))},
            false == l:and_(l:append(Ts,Fs))).

repF(N)-> fun(X)-> l:replicate(N,X) end.
