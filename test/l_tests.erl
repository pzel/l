-module(l_tests).

-include_lib("proper_eunit/include/pt_proper_eunit.hrl").

%%
%% Basic functions
%%

%% append/2
prop_append_empty_left() ->
    ?FORALL(Xs, list(), l:append([], Xs) == Xs).
prop_append_empty_right() ->
    ?FORALL(Xs, list(), l:append(Xs, []) == Xs).
append_two_singletons_test() ->
    ?assertEqual([1,2], l:append([1], [2])).
prop_append_inductive_left() ->
    ?FORALL({Xs,Ys}, {non_empty(list()),non_empty(list())},
            l:append(Xs, Ys) ==
                l:append([l:head(Xs)], l:append(l:tail(Xs), Ys))).
prop_append_inductive_right() ->
    ?FORALL({Xs,Ys}, {non_empty(list()),non_empty(list())},
               l:append(Xs, Ys) ==
                   l:append(Xs, l:append([l:head(Ys)], l:tail(Ys)))).

%% head/1
head_of_empty_list_test() ->
    ?assertError(badarg, l:head([])).
head_of_singleton_test() ->
    ?assertEqual(1, l:head([1])).
prop_head_is_de_cons() ->
    ?FORALL({X,Xs}, {term(), non_empty(list())},
            l:head([X|Xs]) == X).

%% last/1
last_of_empty_list_test() ->
    ?assertError(badarg, l:last([])).
last_of_singleton_test() ->
    ?assertEqual(1, l:last([1])).
last_of_list_test() ->
    ?assertEqual(3, l:last([1,2,3])).

%% tail/1
tail_of_empty_list_test() ->
    ?assertError(badarg, l:tail([])).
tail_of_singleton_test() ->
    ?assertEqual([], l:tail([1])).
prop_tail_is_compliment_of_de_cons() ->
    ?FORALL({X,Xs}, {term(), non_empty(list())},
            l:tail([X|Xs]) == Xs).

%% init/1
init_of_empty_list_test() ->
    ?assertError(badarg, l:init([])).

prop_init_is_rev_tl_rev() ->
    ?FORALL(Xs, non_empty(list()),
            l:init(Xs) == lists:reverse(tl(lists:reverse(Xs)))).

%% null/1
null_of_empty_list_test() ->
    ?assertEqual(true, l:null([])).
prop_null_of_nonempty_is_false() ->
    ?FORALL(Xs, non_empty(list()),
            l:null(Xs) == false).

%% length/1
length_of_empty_list_test() ->
    ?assertEqual(0, l:length([])).
prop_length_of_nonempty_inductive() ->
    ?FORALL(Xs, non_empty(list()),
            l:length(Xs) == 1 + l:length(tl(Xs))).


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
    ?FORALL(Xs, matrix(integer()),
            l:transpose(l:transpose(Xs)) == Xs).
prop_length_transpose_is_length_head() ->
    ?FORALL(Xs, matrix(integer()),
            l:length(l:transpose(Xs)) == length(hd(Xs))).


%% concat/1
concat_empty_test() ->
    ?assertEqual([], l:concat("")).
concat_one_test() ->
    ?assertEqual("a", l:concat(["a"])).
concat_two_test() ->
    ?assertEqual("ab", l:concat(["a", "b"])).
concat_three_test() ->
    ?assertEqual("abc", l:concat(["a", "b", "c"])).

%%
%%  Type generators
%%

matrix(T) ->
    ?LET({Width, Height}, {pos_integer(), pos_integer()},
         [ vector(Height, T) || _W <- lists:seq(1,Width) ]).
