-module(set_operations_tests).
-include_lib("proper_eunit/include/pt_proper_eunit.hrl").

%%
%%  "Set" operations
%%


%% delete/2
prop_delete_in_empty_list() ->
    ?FORALL(X, integer(),
            l:delete(X, []) == []).
delete_only_test() ->
    ?assertEqual([], l:delete(1, [1])).

delete_second_test() ->
    ?assertEqual([2], l:delete(3, [2,3])).

delete_banana_test() ->
    ?assertEqual("bnana", l:delete($a, "banana")).

prop_delete_cons() ->
    ?FORALL({X,Xs}, {integer(), list(integer())},
            l:delete(X, [X|Xs]) == Xs).
