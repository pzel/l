-module(helpers).
-include_lib("triq/include/triq.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([tq/1]).
-export([const/1,
         id/0]).
-export([matrix/1,
        short_list/1]).
-export([factorial/1]).

tq(Prop) -> ?_assert(triq:check(Prop,[],20)).


%% higher order functions

const(X) -> fun(_) -> X end.
id() -> fun(X) -> X end.

%% Type generators

matrix(T) ->
    ?LET({Width, Height}, {pos_integer(), pos_integer()},
         [ vector(Height, T) || _W <- lists:seq(1,Width) ]).

short_list(T) ->
    ?LET(Length, choose(1,10),
         vector(Length, T)).


%% An actually useful factorial!

factorial(0) -> 1;
factorial(1) -> 1;
factorial(X) -> factorial(X-1) * X.
