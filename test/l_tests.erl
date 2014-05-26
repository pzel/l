-module(l_tests).

-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(PROP(A), ?_assert(proper:quickcheck(A()))).

%%
%% Basic functions
%%

%% append/2
append_empty_left() ->
    ?FORALL(Xs, list(), l:append([], Xs) == Xs).
append_empty_right() ->
    ?FORALL(Xs, list(), l:append(Xs, []) == Xs).
append_two_singletons_test() ->
    ?assertMatch([1,2], l:append([1], [2])).
append_inductive_left() ->
    ?FORALL({Xs,Ys}, {non_empty(list()),non_empty(list())},
            l:append(Xs, Ys) ==
                l:append([l:head(Xs)], l:append(l:tail(Xs), Ys))).
append_inductive_right() ->
    ?FORALL({Xs,Ys}, {non_empty(list()),non_empty(list())},
            l:append(Xs, Ys) ==
                l:append(Xs, l:append([l:head(Ys)], l:tail(Ys)))).

%% head/1
head_of_empty_list_test() ->
    ?assertError(badarg, l:head([])).
head_of_singleton_test() ->
    ?assertMatch(1, l:head([1])).
head_is_de_cons() ->
    ?FORALL({X,Xs}, {term(), non_empty(list())},
            l:head([X|Xs]) == X).

%% last/1
last_of_empty_list_test() ->
    ?assertError(badarg, l:last([])).
last_of_singleton_test() ->
    ?assertMatch(1, l:last([1])).
last_of_list_test() ->
    ?assertMatch(3, l:last([1,2,3])).

%% tail/1
tail_of_empty_list_test() ->
    ?assertError(badarg, l:tail([])).
tail_of_singleton_test() ->
    ?assertMatch([], l:tail([1])).
tail_is_compliment_of_de_cons() ->
    ?FORALL({X,Xs}, {term(), non_empty(list())},
            l:tail([X|Xs]) == Xs).

%% init/1
init_of_empty_list_test() ->
    ?assertError(badarg, l:init([])).

init_is_rev_tl_rev() ->
    ?FORALL(Xs, non_empty(list()),
            l:init(Xs) == lists:reverse(tl(lists:reverse(Xs)))).

%% null/1
null_of_empty_list_test() ->
    ?assertMatch(true, l:null([])).
null_of_nonempty_is_false() ->
    ?FORALL(Xs, non_empty(list()),
            l:null(Xs) == false).

%% length/1
length_of_empty_list_test() ->
    ?assertMatch(0, l:length([])).
length_of_nonempty_inductive() ->
    ?FORALL(Xs, non_empty(list()),
            l:length(Xs) == 1 + l:length(tl(Xs))).


%%
%% List transformations
%%

%% map/2
map_of_empty_test() ->
    ?assertMatch([], l:map(fun(X)-> X end, [])).
map_inductive() ->
    F = fun(X) -> X+1 end,
    ?FORALL(Xs, non_empty(list(integer())),
            l:map(F, Xs) == [F(hd(Xs)) | l:map(F,tl(Xs))]).

%% reverse/1
reverse_of_empty_test() ->
    ?assertMatch([], l:reverse([])).
reverse_of_singleton_test() ->
    ?assertMatch([1], l:reverse([1])).
reverse_inductive1() ->
    ?FORALL(Xs, non_empty(list()),
            l:reverse(tl(Xs)) ++ [hd(Xs)] == l:reverse(Xs)).
reverse_twice_is_id() ->
    ?FORALL(Xs, non_empty(list()),
            l:reverse(l:reverse(Xs)) == Xs).


%% intersperse/2
intersperse_empty_test() ->
    ?assertMatch([], l:intersperse(0,[])).
intersperse_one_test() ->
    ?assertMatch([1], l:intersperse(0,[1])).
intersperse_two_test() ->
    ?assertMatch([1,0,2], l:intersperse(0,[1,2])).
intersperse_length() ->
    ?FORALL(Xs, non_empty(list()),
            length(l:intersperse(hd(Xs),Xs)) == length(Xs) * 2 - 1).

%% intercalate/2
intercalate_empty_test() ->
    ?assertMatch([], l:intercalate([], [])).
intercalate_one_test() ->
    ?assertMatch([7], l:intercalate([1], [[7]])).
intercalate_two_test() ->
    ?assertMatch([7,1,8], l:intercalate([1], [[7],[8]])).


%% concat/1
concat_empty_test() ->
    ?assertMatch([], l:concat("")).
concat_one_test() ->
    ?assertMatch("a", l:concat(["a"])).
concat_two_test() ->
    ?assertMatch("ab", l:concat(["a", "b"])).
concat_three_test() ->
    ?assertMatch("abc", l:concat(["a", "b", "c"])).


%%
%% Property instantiation
%%

basic_properties_test_() ->
    [?PROP(append_empty_left),
     ?PROP(append_empty_right),
     ?PROP(append_inductive_left),
     ?PROP(append_inductive_right),
     ?PROP(head_is_de_cons),
     ?PROP(tail_is_compliment_of_de_cons),
     ?PROP(map_inductive),
     ?PROP(reverse_inductive1),
     ?PROP(reverse_twice_is_id),
     ?PROP(intersperse_length),
     ?PROP(init_is_rev_tl_rev),
     ?PROP(null_of_nonempty_is_false),
     ?PROP(length_of_nonempty_inductive)
    ].
