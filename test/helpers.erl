-module(helpers).
-include_lib("proper_eunit/include/pt_proper_eunit.hrl").

-export([matrix/1,
        short_list/1]).
-export([factorial/1]).

%%
%%  Type generators
%%

matrix(T) ->
    ?LET({Width, Height}, {pos_integer(), pos_integer()},
         [ vector(Height, T) || _W <- lists:seq(1,Width) ]).

short_list(T) ->
    ?LET(Length, choose(1,10),
         vector(Length, T)).


%%
%%  Actually useful factorial!
%%

factorial(0) -> 1;
factorial(1) -> 1;
factorial(X) -> factorial(X-1) * X.
