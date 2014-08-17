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


%% foldr/2

foldr2_badarg_test() ->
    F = fun(X,Y) -> X + Y end,
    ?assertError(badarg,
                 l:foldr(F, [])).

prop_foldr2_is_foldr_on_last_element() ->
    ?FORALL({F, List},
            {function([integer(), integer()], integer),
             non_empty(list(integer()))},
            l:foldr(F, List) == l:foldr(F,
                                         l:last(List),
                                         l:init(List))).

%% foldl/3
%% Welcome to the Twilight Zone.
prop_foldl_in_terms_of_foldr() ->
    ?FORALL({F, A, Bs},
            {function([integer(), integer()], integer),
             integer(),
             non_empty(list(integer()))},
            l:foldl(F, A, Bs) ==
                (l:foldr(fun(B,G)-> fun(X)-> G(F(X,B)) end end,
                         fun id/1,
                         Bs))(A)
           ).

foldl_badarg1_test() ->
    ?assertError(badarg,
                 l:foldl(notfun, 1, [1,2,3])).

foldl_badarg2_test() ->
    ?assertError(badarg,
                 l:foldl(fun combine/2, 1, notlist)).

%% foldl/2
foldl2_badarg_test() ->
    ?assertError(badarg,
                 l:foldl(fun combine/2, [])).

prop_foldl2_is_foldl_on_last_element() ->
    ?FORALL({F, List},
            {function([integer(), integer()], integer),
             non_empty(list(integer()))},
            l:foldl(F, List) == l:foldl(F,
                                        l:last(List),
                                        l:init(List))).

id(X) -> X.
combine(X,Y) -> {X,Y}.

%% Hutton[99] :
%% A tutorial on the universality and expressiveness of fold
%% GRAHAM HUTTON
%% http://www.cs.nott.ac.uk/~gmh/fold.pdf
