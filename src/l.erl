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
        ,'and'/1
        ,'or'/1
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
        ,drop_while_end/2
        ,span/2
        ,break/2

        ,filter/2

        ,delete/2
        ]).

%% API
-type pred(A) :: fun((A)->boolean()).

-spec append(list(), list()) -> list().
append(L1,L2)                -> L1 ++ L2.

-spec head([A]) -> A | none().
head(L)         -> erlang:hd(L).

-spec last([A]) -> A | none().
last(L)         -> l:head(l:reverse(L)).

-spec tail([A]) -> [A] | none().
tail(L)         -> erlang:tl(L).

-spec init([A]) -> [A] | none().
init(L)         -> l:reverse(l:tail(l:reverse(L))).

-spec null(list()) -> boolean().
null([])           -> true;
null([_|_])        -> false.

-spec length(list()) -> non_neg_integer().
length(L)            -> erlang:length(L).

-spec map(fun((A)->B),[A]) -> [B].
map(F,L)                   -> [F(X) || X <- L].

-spec reverse([A]) -> [A].
reverse(L)             -> reverse(L,[]).
reverse([], Acc)       -> Acc;
reverse([H|T], Acc)    -> reverse(T, [H|Acc]).

-spec intersperse(A, [A]) -> [A].
intersperse(_,[])            -> [];
intersperse(_,[Last])        -> [Last];
intersperse(E,[H|T])         -> [H,E|intersperse(E,T)].

-spec intercalate([A], [[A]]) -> [A].
intercalate(E,L)              -> concat(intersperse(E,L)).

-spec transpose([[A]])    -> ([[A]]).
transpose(Ls = [[_|_]|_]) -> [ map(fun head/1,Ls) | transpose(map(fun tail/1,Ls)) ];
transpose([[]|_])         -> [];
transpose([])             -> [].

-spec subsequences([A]) -> list([A]).
subsequences([])        -> [[]];
subsequences([H|T]) ->
    Subseqs = subsequences(T),
    append(Subseqs, map(fun(Subseq) -> [ H | Subseq ] end,
                        Subseqs)).

-spec permutations([A]) -> [[A]].
permutations([])        -> [[]];
permutations([El])      -> [[El]];
permutations(List)      ->
    F = fun(El)-> [ [El|Rest] || Rest <- permutations(delete(El, List)) ] end,
    concat(map(F,List)).

-spec fold(fun((A,B) -> B), B, [A]) -> B.
fold(_,V,[])                            -> V;
fold(F,V,[H|T]) when is_function(F,2)   -> F(H, fold(F,V,T));
fold(_,_,_)                             -> error(badarg).

%% @doc This is just an alias
-spec foldr(fun((A,B) -> B), B, [A]) -> B.
foldr(F,V,L)                             -> fold(F,V,L).

-spec foldr(fun((A,A) -> A), [A]) -> A.
foldr(_,[])                           -> error(badarg);
foldr(_,[H|[]])                       -> H;
foldr(F,[H|T])                        -> F(H, foldr(F, T)).

-spec foldl(fun((B,A) -> B), B, [A]) -> B.
foldl(_,V,[])                            -> V;
foldl(F,V,[H|T]) when is_function(F,2)   -> foldl(F, F(V,H), T);
foldl(_,_,_)                             -> error(badarg).

-spec foldl(fun((_,_) -> A),[A,...])      -> A.
foldl(_, [])                              -> error(badarg);
foldl(F,[H1|[]]) when is_function(F,2)    -> H1;
foldl(F,[H1,H2|T]) when is_function(F,2)  -> foldl(F, F(H1,H2), T).

-spec filter(pred(A),[A]) -> [A].
filter(F, Xs)              -> [ X || X <- Xs, F(X) == true ].

-spec delete(A,[A]) -> [A].
delete(X,Xs)        -> delete_(X,Xs,[]).

-spec delete_(A,[A],[A]) -> [A].
delete_(X,[X|T],Acc)     -> append(reverse(Acc), T);
delete_(X,[Y|T],Acc)     -> delete_(X, T, [Y|Acc]);
delete_(_,[],Acc)        -> reverse(Acc).

-spec concat([[A]])              -> [A].
concat([])                       -> [];
concat([L]) when is_list(L)      -> L;
concat([L1|LS]) when is_list(L1) -> append(L1, concat(LS)).

-spec concat_map(fun((A)->[B]),[A])  -> [B].
concat_map(F, L) when is_function(F) -> concat(map(F, L)).

-spec 'and'(list(boolean())) -> boolean().
'and'([])                    -> true;
'and'([false|_])             -> false;
'and'([true|Rest])           -> 'and'(Rest);
'and'(_)                     -> error(badarg).

-spec 'or'(list(boolean())) -> boolean().
'or'([])                    -> false;
'or'([true|_])              -> true;
'or'([false|Rest])          -> 'or'(Rest);
'or'(_)                     -> error(badarg).

-spec any(pred(A),[A])            -> boolean().
any(F,[])    when is_function(F,1) -> false;
any(F,[H|T]) when is_function(F,1) ->
    F(H) orelse any(F, T);
any(_,_)                           -> error(badarg).

-spec all(pred(A),[A])            -> boolean().
all(F,[])    when is_function(F,1) -> true;
all(F,[H|T]) when is_function(F,1) ->
    F(H) andalso all(F, T);
all(_,_)                           -> error(badarg).

-spec sum([A]) -> A.
sum(L)         -> fold(fun erlang:'+'/2, 0, L).

-spec product([A]) -> A.
product(L)         -> fold(fun erlang:'*'/2, 1, L).

-spec maximum([A,...])     -> A.
maximum(L) when is_list(L) -> foldr(fun erlang:max/2, L);
maximum(_)                 -> error(badarg).

-spec minimum([A,...])     -> A.
minimum(L) when is_list(L) -> foldr(fun erlang:min/2, L);
minimum(_)                 -> error(badarg).

-spec replicate(non_neg_integer(),A)    -> [A].
replicate(N,X) when is_integer(N), N>=0 -> replicate_(N,X,[]);
replicate(N,_) when is_integer(N), N<0  -> error(badarg).

-spec replicate_(non_neg_integer(),A,[A]) -> [A].
replicate_(0,_,Acc)                       -> Acc;
replicate_(N,X,Acc)                       -> replicate_(N-1,X,[X|Acc]).

-spec take(integer(),[A]) -> [A].
take(N, L) when is_integer(N), is_list(L) -> take_(N, L, []);
take(_,_)                                 -> error(badarg).

-spec take_(integer(),[A],[A]) -> [A].
take_(_, [], Acc)              -> reverse(Acc);
take_(0, _, Acc)               -> reverse(Acc);
take_(N, [H|T], Acc)           -> take_(N-1, T, [H|Acc]).

-spec drop(integer(),[A]) -> [A].
drop(N, L) when is_integer(N), is_list(L) -> drop_(N, L);
drop(_, _)                                -> error(badarg).

-spec drop_(integer(),[A]) -> [A].
drop_(N, L) when (N<1)     -> L;
drop_(_, [])               -> [];
drop_(N, [_|T])            -> drop_(N-1, T).

-spec split_at(integer(), list()) -> {list(), list()}.
split_at(N, L) when
      is_integer(N), is_list(L)   -> split_at_(N, L, {[], []});
split_at(_,_)                     -> error(badarg).

-spec split_at_(integer(), list(), {list(), list()}) -> {list(), list()}.
split_at_(N, L, _) when N < 0                        -> {[], L};
split_at_(0, L, {Pre, []})                           -> {reverse(Pre), L};
split_at_(_, [],{Pre, Post})                         -> {reverse(Pre), Post};
split_at_(N, [H|T], {Pre, []})                       -> split_at_(N-1, T, {[H|Pre], []}).

-spec take_while(pred(A),[A]) -> [A].
take_while(P, L) when
      is_function(P,1),is_list(L) -> take_while_(P, L, []);
take_while(_, _)                  -> error(badarg).

-spec take_while_(pred(A),[A],[A]) -> [A].
take_while_(_, [], Acc)             -> reverse(Acc);
take_while_(P, [H|T], Acc) ->
    case P(H) of true -> take_while_(P, T, [H|Acc]);
                false -> reverse(Acc)
    end.

-spec drop_while(pred(A),[A]) -> [A].
drop_while(P, L) when
      is_function(P,1),is_list(L) -> drop_while_(P, L);
drop_while(_,_)                   -> error(badarg).

-spec drop_while_(pred(A),[A]) -> [A].
drop_while_(_, [])              -> [];
drop_while_(P, [H|T]) ->
    case P(H) of true -> drop_while_(P, T);
                 false -> [H|T]
    end.

-spec drop_while_end(pred(A),[A]) -> [A].
drop_while_end(P,L) when
      is_list(L), is_function(P,1) -> drop_while_end_(P,L,[],[]);
drop_while_end(_,_)                -> error(badarg).

-spec drop_while_end_(pred(A),[A],[A],[A]) -> [A].
drop_while_end_(_,[],Visited,Qualified) ->
    l:reverse(l:drop(l:length(Qualified), Visited));
drop_while_end_(P,[H|T],V,Q) ->
    case P(H) of true -> drop_while_end_(P,T,[H|V],[H|Q]);
                false -> drop_while_end_(P,T,[H|V],[])
    end.

-spec span(pred(A), [A]) -> {[A], [A]}.
span(P,L) when
      is_list(L), is_function(P,1) -> span_(P,L,[]);
span(_,_) -> error(badarg).

-spec span_(pred(A), [A], [A]) -> {[A], [A]}.
span_(P,[H|T]=L,Acc) ->
    case P(H) of true -> span_(P,T,[H|Acc]);
                 false -> {reverse(Acc),L}
    end;
span_(_,[],Acc) -> {reverse(Acc),[]}.

-spec break(pred(A), [A]) -> {[A], [A]}.
break(P,L) when
      is_list(L), is_function(P,1) -> break_(P,L,[]);
break(_,_) -> error(badarg).

-spec break_(pred(A), [A], [A]) -> {[A], [A]}.
break_(P,[H|T]=L,Acc) ->
    case P(H) of false -> break_(P,T,[H|Acc]);
                 true -> {reverse(Acc), L}
    end;
break_(_,[],Acc) -> {reverse(Acc),[]}.

