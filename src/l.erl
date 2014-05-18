-module(l).
-export([append/2,
         head/1
        ]).


%% API

-spec append(list(), list()) -> list().
append(L1,L2) -> L1 ++ L2.


-spec head(list(A)) -> A | none().
head(L) -> erlang:hd(L).

