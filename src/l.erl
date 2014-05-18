-module(l).
-export([append/2,
         head/1,
         last/1,

         reverse/1
        ]).


%% API

-spec append(list(), list()) -> list().
append(L1,L2) -> L1 ++ L2.

-spec head(list(A)) -> A | none().
head(L) -> erlang:hd(L).

-spec last(list(A)) -> A | none().
last(L) -> l:head(l:reverse(L)).



-spec reverse(list(A)) -> list(A).
reverse(A) -> reverse(A,[]).
reverse([], Acc) -> Acc;
reverse([H|T], Acc) -> reverse(T, [H|Acc]).

