-module(l).
-export([append/2]).


%% API

-spec append(list(), list()) -> list().
append(L1,L2) -> L1 ++ L2.



