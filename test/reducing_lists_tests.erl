-module(reducing_lists_tests).
-include_lib("proper_eunit/include/pt_proper_eunit.hrl").

%%
%%  Reducing lists (folds) tests
%%


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

%% Hutton[99] :
%% A tutorial on the universality and expressiveness of fold
%% GRAHAM HUTTON
%% http://www.cs.nott.ac.uk/~gmh/fold.pdf
