-module(l).
-export([append/2
        ,head/1
        ,last/1
        ,tail/1
        ,init/1
        ,null/1
        ,length/1

        ,map/2
        ,reverse/1
        ,intersperse/2
        ,intercalate/2
        ,transpose/1
        ,subsequences/1
        ,permutations/1

        ,fold/3
        ,foldr/3
        ,foldr/2  %% called foldr1 in Data.List
        ,foldl/3
        ,foldl/2  %% called foldl1 in Data.List

        ,concat/1
        ,concat_map/2
        ,and_/1
        ,or_/1
        ,any/2
        ,all/2
        ,sum/1
        ,product/1
        ,maximum/1
        ,minimum/1

        ,replicate/2

        ,take/2
        ,drop/2
        ,split_at/2
        ,take_while/2
        ,drop_while/2

        ,filter/2

        ,delete/2
        ]).

%% API

-define(pred(T), fun((T)->boolean())).

-spec append(list(), list()) -> list().
append(L1,L2)                -> L1 ++ L2.

-spec head(list(A)) -> A | none().
head(L)             -> erlang:hd(L).

-spec last(list(A)) -> A | none().
last(L)             -> l:head(l:reverse(L)).

-spec tail(list(A)) -> list(A) | none().
tail(L)              -> erlang:tl(L).

-spec init(list(A)) -> list(A) | none().
init(L)             -> l:reverse(l:tail(l:reverse(L))).

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
    concat(map(F,List)).

-spec fold(fun((A,B) -> B), B, list(A)) -> B.
fold(_,V,[])                            -> V;
fold(F,V,[H|T]) when is_function(F,2)   -> F(H, fold(F,V,T));
fold(_,_,_)                             -> error(badarg).

%% @doc This is just an alias
-spec foldr(fun((A,B) -> B), B, list(A)) -> B.
foldr(F,V,L)                             -> fold(F,V,L).

-spec foldr(fun((A,A) -> A), list(A)) -> A.
foldr(_,[])                           -> error(badarg);
foldr(_,[H|[]])                       -> H;
foldr(F,[H|T])                        -> F(H, foldr(F, T)).

-spec foldl(fun((B,A) -> B), B, list(A)) -> B.
foldl(_,V,[])                            -> V;
foldl(F,V,[H|T]) when is_function(F,2)   -> foldl(F, F(V,H), T);
foldl(_,_,_)                             -> error(badarg).

-spec foldl(fun((_,_) -> A),[A,...])      -> A.
foldl(_, [])                              -> error(badarg);
foldl(F,[H1|[]]) when is_function(F,2)    -> H1;
foldl(F,[H1,H2|T]) when is_function(F,2)  -> foldl(F, F(H1,H2), T).

-spec filter(?pred(A), list(A)) -> list(A).
filter(F, Xs)                   -> [ X || X <- Xs, F(X) == true ].

-spec delete(A, list(A)) -> list(A).
delete(X,Xs)             -> deleteW(X,Xs,[]).

-spec deleteW(A, list(A), list(A)) -> list(A).
deleteW(X,[X|T],Acc)               -> append(reverse(Acc), T);
deleteW(X,[Y|T],Acc)               -> deleteW(X, T, [Y|Acc]);
deleteW(_,[],Acc)                  -> reverse(Acc).

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

-spec or_(list(boolean())) -> boolean().
or_([])                    -> false;
or_([true|_])              -> true;
or_([false|Rest])          -> or_(Rest);
or_(_)                     -> error(badarg).

-spec any(?pred(A), list(A))       -> boolean().
any(F,[])    when is_function(F,1) -> false;
any(F,[H|T]) when is_function(F,1) ->
    F(H) orelse any(F, T);
any(_,_)                           -> error(badarg).

-spec all(?pred(A), list(A))       -> boolean().
all(F,[])    when is_function(F,1) -> true;
all(F,[H|T]) when is_function(F,1) ->
    F(H) andalso all(F, T);
all(_,_)                           -> error(badarg).

-spec sum(list(A)) -> A.
sum(L)             -> fold(fun erlang:'+'/2, 0, L).

-spec product(list(A)) -> A.
product(L)             -> fold(fun erlang:'*'/2, 1, L).

-spec maximum([A,...])     -> A.
maximum(L) when is_list(L) -> foldr(fun erlang:max/2, L);
maximum(_)                 -> error(badarg).

-spec minimum([A,...])     -> A.
minimum(L) when is_list(L) -> foldr(fun erlang:min/2, L);
minimum(_)                 -> error(badarg).

-spec replicate(non_neg_integer(), A) -> list(A).
replicate(N,X) when is_integer(N), N>=0 -> replicateW(N,X,[]);
replicate(N,_) when is_integer(N), N<0 -> error(badarg).

-spec replicateW(non_neg_integer(), T, list(T)) -> list(T).
replicateW(0,_,Acc)                             -> Acc;
replicateW(N,X,Acc)                             -> replicateW(N-1,X,[X|Acc]).

-spec take(integer(), list(T))  -> list(T).
take(N, L) when
      is_integer(N), is_list(L) -> takeW(N, L, []);
take(_,_)                       -> error(badarg).

-spec takeW(integer(), list(T), list(T)) -> list(T).
takeW(_, [], Acc)                        -> reverse(Acc);
takeW(0, _, Acc)                         -> reverse(Acc);
takeW(N, [H|T], Acc)                     -> takeW(N-1, T, [H|Acc]).

-spec drop(integer(), list(T))  -> list(T).
drop(N, L) when
      is_integer(N), is_list(L) -> dropW(N, L);
drop(_, _)                      -> error(badarg).

-spec dropW(integer(), list(T)) -> list(T).
dropW(N, L) when (N<1)          -> L;
dropW(_, [])                    -> [];
dropW(N, [_|T])                 -> dropW(N-1, T).

-spec split_at(integer(), list()) -> {list(), list()}.
split_at(N, L) when
      is_integer(N), is_list(L)   -> split_atW(N, L, {[], []});
split_at(_,_)                     -> error(badarg).


-spec split_atW(integer(), list(), {list(), list()}) -> {list(), list()}.
split_atW(N, L, _) when N < 0                        -> {[], L};
split_atW(0, L, {Pre, []})                           -> {reverse(Pre), L};
split_atW(_, [],{Pre, Post})                         -> {reverse(Pre), Post};
split_atW(N, [H|T], {Pre, []})                       -> split_atW(N-1, T, {[H|Pre], []}).

-spec take_while(?pred(A), list(A)) -> list(A).
take_while(P, L) when
      is_function(P, 1), is_list(L) -> take_whileW(P, L, []);
take_while(_, _)                    -> error(badarg).

-spec take_whileW(?pred(A), list(A), list(A)) -> list(A).
take_whileW(_, [], Acc)                       -> reverse(Acc);
take_whileW(P, [H|T], Acc) ->
    case P(H) of true -> take_whileW(P, T, [H|Acc]);
                false -> reverse(Acc)
    end.

-spec drop_while(?pred(A), list(A)) -> list(A).
drop_while(P, L) when
      is_function(P), is_list(L)    -> drop_whileW(P, L);
drop_while(_,_)                     -> error(badarg).

-spec drop_whileW(?pred(A), list(A)) -> list(A).
drop_whileW(_, [])                   -> [];
drop_whileW(P, [H|T]) ->
    case P(H) of true -> drop_whileW(P, T);
                 false -> [H|T]
    end.
