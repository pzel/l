-module(infinite_lists_tests).
-include_lib("proper_eunit/include/pt_proper_eunit.hrl").

%%
%%  (Not particulary) Infinite lists
%%

%% replicate/2
replicate_test_() ->
    [?_assertError(badarg, l:replicate(-1, a)),
     ?_assertEqual([], l:replicate(0, a))
     ].
prop_replicate_identity() ->
     ?FORALL({N, X}, {non_neg_integer(), term()},
             l:all(eqF(X), l:replicate(N,X))).
prop_replicate_length() ->
     ?FORALL({N, X}, {non_neg_integer(), term()},
             N == length(l:replicate(N,X))).

eqF(P)->
    fun(Q)-> P =:= Q end.
