l
=
[![Build
Status](https://travis-ci.org/pzel/l.svg?branch=master)](https://travis-ci.org/pzel/l)

Implementing Haskell's [Data.List](http://hackage.haskell.org/package/base-4.6.0.1/docs/Data-List.html) in Erlang.

What is the point of this library?
----------------------------------

Haskell provides a very intuitive and feature-complete list operation library,
and I believe Erlang programmers could benefit from adopting this common
idiomatic core. 

Also: having to implement little 'helper functions' for list manipulation in
random places in the codebase is suboptimal. Helper functions smell, and the
remedy is code removal via better abstractions.

___________________
### Basic functions


#### append(list(), list()) -> list().

Take two lists (L1,L2)  and glue L2 to the end of L1.

    [1,2] = l:append([1], [2]).


#### head ([A]) -> A.

Return the first element of a non-empty list.

    a = l:head([a,b,c]).  
    {'EXIT',{badarg,_}} = (catch l:head([])).


#### last ([A]) -> A.

Return the last element of a non-empty list.

    c = l:last([a,b,c]).  
    {'EXIT',{badarg,_}} = (catch l:last([])).


#### tail ([A]) -> [A].

Return everything but the head of a non-empty list.

    [b,c] = l:tail([a,b,c]).
    [] = l:tail([1]).
    {'EXIT',{badarg,_}} = (catch l:tail([])).


#### init ([A]) -> [A].

Return everything *but* the last element of a non-empty list.

    [a,b] = l:init([a,b,c]).
    {'EXIT',{badarg,_}} = (catch l:init([])).


#### null (list()) -> boolean().

Answer the question: "is this an empty list?"

    false = l:null([a,b,c]).
    true = l:null([]).
    {'EXIT',{badarg,_}} = (catch l:null([])).


#### length (list()) -> non\_neg\_integer().

Return the length of the list.

    3 = l:length([a,b,c]).
    0 = l:length([]).
    {'EXIT',{badarg,_}} = (catch l:length(not_list)).


________________________
### List transformations


#### map (fun((A)->B), [A]) -> [B].

Create a new list by applying the function to every element of the argument list.

    Add1 = fun(I) -> I + 1 end.
    [2,3,4] = l:map(Add1, [1,2,3]).

    BadFun = fun(X,Y) -> X + Y end.
    {'EXIT', {{badarity, _},_}} = (catch l:map(BadFun, [1,2,3])).

    [] = l:map(Add1, []).


#### reverse ([A]) -> [A].

Reverse the elements of the list.

    [c,b,a] = l:reverse([a,b,c]).
    [] = l:reverse([]).
    {'EXIT',{badarg,_}} = (catch l:reverse(not_list)).


#### intersperse (A, [A]) -> [A].

Place the single element A in between all elements of [A].

    [] = l:intersperse(z, []).
    [a] = l:intersperse(z, [a]).
    [a,z,b] = l:intersperse(z, [a,b]).
    [a,z,b,z,c] = l:intersperse(z, [a,b,c]).

    {'EXIT',{badarg,_}} = (catch l:intersperse(z, not_list)).


#### intercalate ([A], [[A]]) -> [A].

Connect the lists in the second argument using the list in the first argument.

    [] = l:intercalate([] , [[], []]).
    [1,0] = l:intercalate([0] , [[1],[]]).
    [1,0,1] = l:intercalate([0] , [[1],[1]]).
    "apple_banana" = l:intercalate("_" , ["apple", "banana"]).
    {'EXIT',{badarg,_}} = (catch l:intercalate([0], [[1], notlist])).
    {'EXIT',{badarg,_}} = (catch l:intercalate(notlist, [[1],[2]])).


#### transpose ([[A]]) -> ([[A]]).

Transpose the rows and columns of the argument. The lengths of the sublists
must be the same.

    [[1,2,3], [1,2,3]] = l:transpose([[1,1], [2,2], [3,3]]).
    [[1,1], [2,2]] = l:transpose(l:transpose([[1,1], [2,2]])).
    

#### subsequences ([A]) -> [[A]].

List all the possible sublists of the argument list. Element ordering does not
matter, i.e. all possible subsets of the elements in the list will be returned.

    [[]] = l:subsequences([]).
    [[], [a]] = l:subsequences([a]).
    [[],[c],[b],[b,c],[a],[a,c],[a,b],[a,b,c]] = l:subsequences([a,b,c]).
    {'EXIT',{badarg,_}} = (catch l:subsequences(notlist)).


#### permutations ([A]) -> [[A]].

List all the possible orderings of the given list.

    [[]] = l:permutations([]).
    [[a]] = l:permutations([a]).
    [[a,b,c],[a,c,b],[b,a,c],[b,c,a],[c,a,b],[c,b,a]] = l:permutations([a,b,c]).
    {'EXIT',{badarg,_}} = (catch l:permutations(notlist)).


__________________________
### Reducing lists (folds)

#### foldr fun ((A,B) -> B), B, [A]) -> B.

Reduce the list of A-typed values to a single B-typed value, using the given
function. The function will first receive the supplied seed value and the
last element of the list.

    Add = fun(A,B) -> A + B end.
    0 = l:foldr(Add, 0, []).
    10 = l:foldr(Add, 0, [1,2,3,4]).
    {'EXIT',{badarg,_}} = (catch l:foldr(notfun, 0, [])).
    {'EXIT',{badarg,_}} = (catch l:foldr(Add, 0, notlist)).

    AddShow = fun(A,B) -> io:format("~p + ~p~n", [A,B]), A+B end.
    6 = l:foldr(AddShow, 0, [1,2,3]).
    %% prints:
      3 + 0
      2 + 3
      1 + 5

#### foldr (fun((A,A) -> A), non\_empty\_list(A)) -> A.

Reduce the non-empty list using the given function.

    Add = fun(A,B) -> A + B end.
    3 = l:foldr(Add, [1,2]).
    {'EXIT',{badarg,_}} = (catch l:foldr(Add, [])).


#### foldl (fun((B,A) -> B), B, [A]) -> B.

Reduce the list of A-typed values to a single B-typed value, using the given
function. The function will first receive the supplied seed value and the first
element of the list.

    Add = fun(A,B) -> A + B end.
    0 = l:foldl(Add, 0, []).
    10 = l:foldl(Add, 0, [1,2,3,4]).
    {'EXIT',{badarg,_}} = (catch l:foldl(notfun, 0, [])).
    {'EXIT',{badarg,_}} = (catch l:foldl(Add, 0, notlist)).

    AddShow = fun(A,B) -> io:format("~p + ~p~n", [A,B]), A+B end.
    6 = l:foldl(AddShow, 0, [1,2,3]).
    %% prints:
      0 + 1
      1 + 2
      3 + 3

#### foldl (fun((A,A) -> A), non_empty_list(A)) -> A.

Reduce the non-empty list using the given function.

    Add = fun(A,B) -> A + B end.
    3 = l:foldl(Add, [1,2]).
    {'EXIT',{badarg,_}} = (catch l:foldl(Add, [])).

_________________
### Special folds


#### concat ([[A]]) -> [A]

Concatenate all elements in the given list-of-lists.

    [] = l:concat([]).
    [] = l:concat([[]]).
    [] = l:concat([ [], [], []]).
    [1,2,3,4,5,6,7] = l:concat([ [1,2,3], [4], [], [5,6,7]]).

Unlike `lists:flatten/1`, only one level of nesting is flattened.

    [1,2,3,[4],5,6,7] = l:concat([ [1,2,3], [[4]], [5,6,7]]).


#### concat_map (fun((A)->[B]),[A])  -> [B].

Apply the function `(A->[B])` to each element of the given list and concatenate the resulting lists.

    Repeat2 = fun(X) -> [X,X] end.
    [1,1,2,2,3,3] = l:concat_map(Repeat2, [1,2,3]).
    "abc" = l:concat_map(fun erlang:atom_to_list/1, [a,b,c]).
    {'EXIT',{badarg,_}} = (catch l:concat_map(notfun, [a,b,c])).
    {'EXIT',{badarg,_}} = (catch l:concat_map(Repeat2, notlist)).


#### 'and' ([boolean()]) -> boolean().

Return the logical conjunction of a list of boolean values.

    true = l:'and'([]).
    false = l:'and'([true, true, false]).
    true =  l:'and'([true, true, true]).
    {'EXIT',{badarg,_}} = (catch l:'and'([true, true, 1, true])).


#### 'or' ([boolean()]) -> boolean().

Return the logical disjunction of a list of boolean values.

    false = l:'or'([]).
    true = l:'or'([false,false,true]).
    {'EXIT',{badarg,_}} = (catch l:'or'([false,false,1,true])).


#### any (fun((A)->boolean()), [A])        -> boolean().

Answer the question: "Does any element in the list satisfy this predicate?"

    LessThanZero = fun(X)-> X < 0 end.
    false = l:any(LessThanZero, []).
    false = l:any(LessThanZero, [1]).
    true = l:any(LessThanZero, [1,-1]).
    {'EXIT',{badarg,_}} = (catch l:any(notfun, [1,-1])).
    {'EXIT',{badarg,_}} = (catch l:any(LessThanZero, notlist)).


#### all (fun((A)->boolean()), [A])        -> boolean().

Answer the question: "Do all elements in the list satisfy this predicate?"

    LessThanZero = fun(X)-> X < 0 end.
    true = l:all(LessThanZero, []).
    false = l:all(LessThanZero, [1,-1]).
    true = l:all(LessThanZero, [-1,-2,-3,-4]).
    {'EXIT',{badarg,_}} = (catch l:all(notfun, [1,-1])).
    {'EXIT',{badarg,_}} = (catch l:all(LessThanZero, notlist)).


#### sum ([number()]) -> number().

Sum the numbers in the list. The empty list sums to 0.

    0 = l:sum([]).
    10 = l:sum([1,2,3,4]).
    {'EXIT',{badarg,_}} = (catch l:sum(notlist)).


#### product ([number()]) -> number().

Multiply the numbers in the list. The empty list produces 1.

    1 = l:product([]).
    24 =  l:product([1,2,3,4]).
    {'EXIT',{badarg,_}} = (catch l:product(notlist)).

#### maximum (non\_empty\_list(A))  -> A.

Return the greatest element in the list. As `erlang:max/2` is used for the comparison, this function will happily compare differently-typed values.  
Please don't rely on this behaviour!

    3 = l:maximum([1,2,3]).
    c = l:maximum([a,b,c]).
    [1,2] = l:maximum([[], [1], [1,2]]).
    "string" = l:maximum([1,atom,"string"]). %% You don't want to do this!
    {'EXIT',{badarg,_}} = (catch l:maximum(notlist)).


#### minimum (non\_empty\_list(A))  -> A.

Return the greatest element in the list. As `erlang:min/2` is used for the comparison, this function will happily compare differently-typed values.  
Please don't rely on this behaviour!

    1 = l:minimum([1,2,3]).
    a = l:minimum([a,b,c]).
    [] = l:minimum([[], [1], [1,2]]).
    1 = l:minimum([1,atom,"string"]). %% This is bogus, TBH
    {'EXIT',{badarg,_}} = (catch l:minimum(notlist)).

__________________
### Infinite lists

        iterate/3
        replicate/2

### Sublists
#### Extracting sublists
        take/2
        drop/2
        split_at/2
        take_while/2
        drop_while/2
        drop_while_end/2
        span/2
        break/2
        strip_prefix/2
        group/1
        inits/1
        tails/1

### Predicates
        is_prefix_of/2
        is_suffix_of/2
        is_infix_of/2

### Searching lists
#### Searching by equality
        elem/2
        not_elem/2
        lookup/2

#### Searching with a predicate
        find/2
        filter/2
        partition/2

### Indexing lists
        index/2 (a.k.a '!!'/2)
        elem_index/2
        elem_indices/2
        find_index/2
        find_indices/2

### Zipping and unzipping lists
        zip/2
        zip_with/3,
        zip3/3,
        zip_with3/4,
        zip4/4,
        zip_with4/5,
        unzip/1
        unzip3/1
        unzip4/1


### Special lists
#### "Set" operations
         nub/1
         delete/2

Project roadmap
---------------

###0.1 – All Data.List functions implemented

Functions dealing specifically with eager vs. lazy evaluation (i.e `foldl'` &
friends) will be omitted. Functions generating infinite lists (`iterate`,
`repeat`, `replicate` and `cycle`) will be dealt with case-by case: dropped
entirely or fitted with an extra parameter specifying the length of the desired
output.

At this stage, no calls to the `lists:` module will be made. The idea is to
bootstrap this library using only on its own functions.

Unit tests and [triq](https://github.com/krestenkrab/triq) properties are
written before the functions are actually implemented, but may be incomplete.

Code coverage doesn't drop below 100%.


###0.2 – All functions typed, documented, and exhaustively tested

At this stage, the module should be verfifed correct to a great
extent. The source will be annotated, and generated documentation available.


###0.3 – Optimization

This version will make the code more suitable for production by optimizing
computationally wasteful implementations. Calls to `lists:` functions may appear
at this point.
