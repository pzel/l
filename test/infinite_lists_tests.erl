-module(infinite_lists_tests).
-include_lib("eunit/include/eunit.hrl").
-include_lib("triq/include/triq.hrl").

%%
%%  (Not particulary) Infinite lists
%%

tq(Prop) -> ?_assert(triq:check(Prop,[],20)).

%% replicate/2
replicate_test_() ->
    [?_assertError(badarg, l:replicate(-1, a)),
     ?_assertEqual([], l:replicate(0, a))
     ].
replicate_identity_test_() ->
     tq(?FORALL({N, X}, {non_neg_integer(), any()},
                l:all(eqF(X), l:replicate(N,X)))).
replicate_length_test_() ->
     tq(?FORALL({N, X}, {non_neg_integer(), any()},
                N == length(l:replicate(N,X)))).

eqF(P)->
    fun(Q)-> P =:= Q end.
