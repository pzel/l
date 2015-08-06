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

Also: I don't like having to implement little 'helper functions' for list
manipulation in random places in the codebase. Helper functions smell, and the
remedy is code removal via better abstractions.


Basic functions
---------------

### append/2

Take two lists (L1,L2)  and glue L2 to the end of L1.

    [1,2] = l:append([1], [2]).


### head/1
Return the first element of a non-empty list.

    a = l:head([a,b,c]).  
    {'EXIT',{badarg,_}} = (catch l:head([])).


### last/1
Return the last element of a non-empty list.

    c = l:last([a,b,c]).  
    {'EXIT',{badarg,_}} = (catch l:last([])).


### tail/1
Return everything but the head of a non-empty list.

    [b,c] = l:tail([a,b,c]).
    [] = l:tail([1]).
    {'EXIT',{badarg,_}} = (catch l:tail([])).

### init/1
Return everything *but* the last element of a non-empty list.

    [a,b] = l:init([a,b,c]).
    {'EXIT',{badarg,_}} = (catch l:init([])).


### null/1
Answer the question: "is this an empty list?"

    false = l:null([a,b,c]).
    true = l:null([]).
    {'EXIT',{badarg,_}} = (catch l:null([])).

### length/1
Return the length of the list.

    3 = l:length([a,b,c]).
    0 = l:length([]).
    {'EXIT',{badarg,_}} = (catch l:length(not_list)).



### List transformations
        map/2
        reverse/1
        intersperse/2
        intercalate/2
        transpose/1
        subsequences/1
        permutations/1

### Reducing lists (folds)
        foldr/3
        foldr/2  (known as foldr1 in Data.List)
        foldl/3
        foldl/2  (known as foldl1 in Data.List)

### Special folds
        concat/1
        concat_map/2
        'and'/1
        'or'/1
        any/1
        all/2
        sum/1
        product/1
        maximum/1
        minimum/1

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
