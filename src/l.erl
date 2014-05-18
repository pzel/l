-module(l).
-export([append/2,
         head/1,
         last/1,
         tail/1,
         init/1,
         reverse/1
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
init(L)             -> init(L,[]).
init([H|Tail], Acc) -> init(Tail, [H|Acc]);
init([], Acc)       -> reverse(tail(Acc)).

-spec reverse(list(A)) -> list(A).
reverse(L)             -> reverse(L,[]).
reverse([], Acc)       -> Acc;
reverse([H|T], Acc)    -> reverse(T, [H|Acc]).
