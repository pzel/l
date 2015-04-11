-module(set_operations_tests).
-include_lib("eunit/include/eunit.hrl").
-include_lib("triq/include/triq.hrl").
-include_lib("helpers.hrl").


%%
%%  "Set" operations
%%


%% delete/2

delete_test_() ->
    [?_assertEqual([],      l:delete(1, [1])),
     ?_assertEqual([2],     l:delete(3, [2,3])),
     ?_assertEqual("bnana", l:delete($a, "banana"))
     ].

delete_in_empty_list_test_() ->
    ?_forall(X, int(), l:delete(X, []) == []).
delete_cons_test_() ->
    ?_forall({X,Xs}, {int(), list(int())}, l:delete(X, [X|Xs]) == Xs).


nub_test_() ->
    [ ?_assertError(badarg, l:nub(notlist)),
      ?_assertEqual([], l:nub([])),
      ?_assertEqual([1], l:nub([1,1])),
      ?_assertEqual([1,2], l:nub([1,2,1,2,1,2,1]))
    ].
