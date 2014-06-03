-module(helpers).
-include_lib("proper_eunit/include/pt_proper_eunit.hrl").

-export([matrix/1]).

%%
%%  Type generators
%%

matrix(T) ->
    ?LET({Width, Height}, {pos_integer(), pos_integer()},
         [ vector(Height, T) || _W <- lists:seq(1,Width) ]).
