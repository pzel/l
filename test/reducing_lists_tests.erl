-module(reducing_lists_tests).
-include_lib("proper_eunit/include/pt_proper_eunit.hrl").

%%
%%  Reducing lists (folds) tests
%%

%% foldr/3

foldr_badarg_test() ->
    ?assertError(badarg,
                 l:foldr(notfun, 2, [1])).

%% foldr is the standard fold ;)
%% Using the univeral property from Hutton[99].

prop_universal() ->
    ?FORALL({F, V, [X|Xs]},
            {function([integer(), integer()], integer),
             integer(),
             non_empty(list(integer()))},
            begin
                G = fun(List)-> l:fold(F, V, List) end,
                G([]) == V
                andalso
                G([X|Xs]) == F(X, G(Xs))
            end).

prop_fold_is_foldr() ->
    %% In case the implementation changes.
    ?FORALL({F, V, Xs},
            {function([integer(), integer()], integer),
             integer(),
             non_empty(list(integer()))},
            l:foldr(F, V, Xs) == l:fold(F, V, Xs)).


%% foldr1/2

foldr1_badarg_test() ->
    F = fun(X,Y) -> X + Y end,
    ?assertError(badarg,
                 l:foldr1(F, [])).

prop_foldr1_is_foldr_on_last_element() ->
    ?FORALL({F, List},
            {function([integer(), integer()], integer),
             non_empty(list(integer()))},
            l:foldr1(F, List) == l:foldr(F,
                                         l:last(List),
                                         l:init(List))).
%% Hutton[99] :
%% A tutorial on the universality and expressiveness of fold
%% GRAHAM HUTTON
%% http://www.cs.nott.ac.uk/~gmh/fold.pdf
