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
         intersperse/2,
         intercalate/2,
         transpose/1,
         subsequences/1,
         permutations/1,

         concat/1,
         concat_map/2,
         and_/1,

         replicate/2,

         filter/2,

         delete/2
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

-spec intercalate(list(A), list(list(A))) -> list(A).
intercalate(E,L)                          -> concat(intersperse(E,L)).

-spec transpose(list(list(A))) -> (list(list(A))).
transpose(Ls = [[_|_]|_])      -> [ map(fun head/1,Ls) | transpose(map(fun tail/1,Ls)) ];
transpose([[]|_])              -> [];
transpose([])                  -> [].

-spec subsequences(list(A)) -> list(list(A)).
subsequences([])             -> [[]];
subsequences([H|T])          ->
    Subseqs = subsequences(T),
    append(Subseqs, map(fun(Subseq) -> [ H | Subseq ] end,
                        Subseqs)).

-spec permutations(list(A)) -> list(list(A)).
permutations([])            -> [[]];
permutations([El])          -> [[El]];
permutations(List)          ->
    F = fun(El)-> [ [El|Rest] || Rest <- permutations(delete(El, List)) ] end,
    concat(lists:map(F,List)).

-spec filter(fun((A)->boolean()), list(A)) -> list(A).
filter(F, Xs)                              ->
    [ X || X <- Xs, F(X) == true ].

-spec delete(A, list(A)) -> list(A).
delete(X,Xs)             -> delete(X,Xs,[]).

%% @doc internal
-spec delete(A, list(A), list(A)) -> list(A).
delete(X,[X|T],Acc)     -> append(reverse(Acc), T);
delete(X,[Y|T],Acc)     -> delete(X, T, [Y|Acc]);
delete(_,[],Acc)        -> reverse(Acc).

-spec concat(list(list(A))) -> list(A).
concat([])                  -> [];
concat([L]) when is_list(L) -> L;
concat([L1|LS]) when is_list(L1) -> append(L1, concat(LS)).

-spec concat_map(fun((A) -> list(B)), list(A)) -> list(B).
concat_map(F, L) when is_function(F) -> concat(map(F, L)).

-spec and_(list(boolean())) -> boolean().
and_([])                    -> true;
and_([false|_])             -> false;
and_([true|Rest])           -> and_(Rest);
and_(_)                     -> error(badarg).


-spec replicate(non_neg_integer(), T) -> list(T).
replicate(N,X) when is_integer(N), N>=0 -> replicate(N,X,[]);
replicate(N,_) when is_integer(N), N<0 -> error(badarg).

%% @doc internal
-spec replicate(non_neg_integer(), T, list(T)) -> list(T).
replicate(0,_,Acc) -> Acc;
replicate(N,X,Acc) -> replicate(N-1,X,[X|Acc]).

