-module(infinite_lists_tests).
-include_lib("proper_eunit/include/pt_proper_eunit.hrl").

%%
%%  (Not particulary) Infinite lists
%%

%% replicate/2
replicate_neg_test() ->
    ?assertError(badarg, l:replicate(-1, a)).
replicate_0_test() ->
    ?assertEqual([], l:replicate(0, a)).
prop_replicate_identity() ->
     ?FORALL({N, X}, {non_neg_integer(), term()},
             all(eqF(X), l:replicate(N,X))).
prop_replicate_length() ->
     ?FORALL({N, X}, {non_neg_integer(), term()},
             N == length(l:replicate(N,X))).



%% @TODO:
%% replace this all with l:all when implemented
all(P,Xs) when is_function(P,1) ->
    Satisfied = [ X || X <- Xs, P(X) ],
    length(Xs) == length(Satisfied).

eqF(P)->
    fun(Q)-> P =:= Q end.
