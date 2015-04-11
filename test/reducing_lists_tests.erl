-module(reducing_lists_tests).
%% TODO Triq doesn't implement function() generators
%% TODO implement & pull request
-include_lib("eunit/include/eunit.hrl").
-include_lib("triq/include/triq.hrl").
-import(helpers, [tq/1, function/2]).


%%
%%  Reducing lists (folds) tests
%%

%% foldr/3

foldr_badarg_test() ->
    ?assertError(badarg,
                 l:foldr(notfun, 2, [1])).

%% foldr is the standard fold ;)
%% Using the univeral property from Hutton[99].

universal_test_() ->
    tq(?FORALL({F, V, [X|Xs]},
               {function([int(), int()], integer),
                int(),
                non_empty(list(int()))},
               begin
                   G = fun(List)-> l:fold(F, V, List) end,
                   G([]) == V
                       andalso
                       G([X|Xs]) == F(X, G(Xs))
               end)).

fold_is_foldr_test_() ->
    %% In case the implementation changes.
    tq(?FORALL({F, V, Xs},
               {function([int(), int()], integer),
                int(),
                non_empty(list(int()))},
               l:foldr(F, V, Xs) == l:fold(F, V, Xs))).


%% foldr/2

foldr2_badarg_test() ->
    F = fun(X,Y) -> X + Y end,
    ?assertError(badarg,
                 l:foldr(F, [])).

foldr2_is_foldr_on_last_element_test_() ->
    tq(?FORALL({F, List},
               {function([int(), int()], integer),
                non_empty(list(int()))},
               l:foldr(F, List) == l:foldr(F,
                                           l:last(List),
                                           l:init(List)))).

%% foldl/3
%% Welcome to the Twilight Zone.
foldl_in_terms_of_foldr_test_() ->
    Id = fun(X)-> X end,
    tq(?FORALL({F, A, Bs},
               {function([int(), int()], integer),
                int(),
                non_empty(list(int()))},
               l:foldl(F, A, Bs) ==
                   (l:foldr(fun(B,G)-> fun(X)-> G(F(X,B)) end end,
                            Id,
                            Bs))(A)
              )).

foldl_test_() ->
    [?_assertError(badarg, l:foldl(notfun, 1, [1,2,3])),
     ?_assertError(badarg, l:foldl(fun(A,B)-> {A,B} end, 1, notlist))
     ].

%% foldl/2
foldl2__test_() ->
    [?_assertError(badarg, l:foldl(fun(A,B)-> {A,B} end, []))].

foldl2_is_foldl_on_last_element_test_() ->
    tq(?FORALL({F, List},
               {function([int(), int()], integer),
                non_empty(list(int()))},
               l:foldl(F, List) == l:foldl(F,
                                           l:last(List),
                                           l:init(List)))).

%% Hutton[99] :
%% A tutorial on the universality and expressiveness of fold
%% GRAHAM HUTTON
%% http://www.cs.nott.ac.uk/~gmh/fold.pdf
