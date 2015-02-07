-module(set_operations_tests).
-include_lib("eunit/include/eunit.hrl").
-include_lib("triq/include/triq.hrl").
-import(helpers, [tq/1]).


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
    tq(?FORALL(X, int(),
               l:delete(X, []) == [])).
delete_cons_test_() ->
    tq(?FORALL({X,Xs}, {int(), list(int())},
               l:delete(X, [X|Xs]) == Xs)).
