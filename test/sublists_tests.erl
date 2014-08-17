-module(sublists_tests).
-include_lib("proper_eunit/include/pt_proper_eunit.hrl").

%% take/2
%%

take_hello_world_test() ->
    ?assertEqual("Hello",
                 l:take(5, "Hello, World!")).

take_sublist_test() ->
    ?assertEqual([1,2,3],
                 l:take(3, [1,2,3,4,5])).

take_with_more_than_length_test() ->
    ?assertEqual([1,2],
                 l:take(3, [1,2])).

take_any_with_empty_list_test() ->
    ?assertEqual([],
                 l:take(3, [])).

take_negative_test() ->
    ?assertEqual([],
                 l:take(-1, [])).

take_zero_test() ->
    ?assertEqual([],
                 l:take(0, [1,3])).

prop_identity() ->
    ?FORALL(L, list(),
            ?FORALL(N, choose(length(L), inf),
                l:take(N, L) == L)).

prop_take_as_reverse_drop() ->
    ?FORALL(L, non_empty(list()),
            ?FORALL(N, choose(0, length(L)),
                    l:take(N,L) ==
                        l:reverse(l:drop(length(L) - N,
                                         l:reverse(L))))).

%% drop/2
drop_hello_world_test() ->
    ?assertEqual("World!",
                 l:drop(6, "Hello World!")).

drop_more_than_test() ->
    ?assertEqual([],
                 l:drop(3, [1,2])).

drop_more_from_empty_test() ->
    ?assertEqual([],
                 l:drop(3, [])).

drop_negative_test() ->
    ?assertEqual([1,2],
                 l:drop(-1, [1,2])).

