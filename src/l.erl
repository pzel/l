-module(l).
-export([append/2,
         head/1,
         last/1,
         tail/1,
         init/1,
         null/1,
         length/1,

         map/2,
         reverse/1,
         intersperse/2
        ]).

%% API

-spec append(list(), list()) -> list().
append(L1,L2)                -> L1 ++ L2.

-spec head(list(A)) -> A | none().
head(L)             -> erlang:hd(L).

-spec last(list(A)) -> A | none().
last(L)             -> l:head(l:reverse(L)).

-spec tail(list(A)) -> list(A) | none().
tail(L)              -> erlang:tl(L).

-spec init(list(A)) -> list(A) | none().
init(L)             -> reverse(tail(reverse(L))).

-spec null(list()) -> boolean().
null([])           -> true;
null([_|_])        -> false.

-spec length(list()) -> non_neg_integer().
length(L)            -> erlang:length(L).

-spec map(fun((A)->B), list(A)) -> list(B).
map(F,L)                        -> [F(X) || X <- L].

-spec reverse(list(A)) -> list(A).
reverse(L)             -> reverse(L,[]).
reverse([], Acc)       -> Acc;
reverse([H|T], Acc)    -> reverse(T, [H|Acc]).

-spec intersperse(A, list(A)) -> list(A).
intersperse(_,[])            -> [];
intersperse(_,[Last])        -> [Last];
intersperse(E,[H|T])         -> [H,E|intersperse(E,T)].
