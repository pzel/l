-module(infinite_lists_tests).
-include_lib("eunit/include/eunit.hrl").
-include_lib("triq/include/triq.hrl").
-include_lib("helpers.hrl").

%%
%%  (Not particulary) Infinite lists
%%


iterate_test_() ->
    [?_assertError(badarg, l:iterate(notint, fun inc/1, 1)),
     ?_assertError(badarg, l:iterate(1, fun add/2, 1)),
     ?_assertEqual([], l:iterate(0, fun inc/1, 1)),
     ?_assertEqual([1,2,3,4], l:iterate(4, fun inc/1, 1))
     ].

%% replicate/2
replicate_test_() ->
    [?_assertError(badarg, l:replicate(-1, a)),
     ?_assertEqual([], l:replicate(0, a))
     ].
replicate_identity_test_() ->
     ?_forall({N, X}, {non_neg_integer(), any()},
                l:all(eqF(X), l:replicate(N,X))).
replicate_length_test_() ->
     ?_forall({N, X}, {non_neg_integer(), any()},
                N == length(l:replicate(N,X))).

eqF(P)->
    fun(Q)-> P =:= Q end.

inc(X) -> X+1.
add(A,B) -> A+B.
