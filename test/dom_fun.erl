-module(dom_fun).
%% move me to triq when I'm ripe and tested!
-include_lib("triq/include/triq.hrl").

-export([function/2]).
-export([neg_integer/0]).

function(InputTypes, OutputType) ->
    ?LET(X, OutputType, return(fun_from_input(InputTypes, X))).

neg_integer() ->
    ?LET(I, pos_integer(), return(-1 * I)).

fun_from_input([_,_,_,_,_,_,_], ReturnVal) -> fun(_,_,_,_,_,_,_) -> ReturnVal end;
fun_from_input([_,_,_,_,_,_], ReturnVal)   -> fun(_,_,_,_,_,_) -> ReturnVal end;
fun_from_input([_,_,_,_,_], ReturnVal)     -> fun(_,_,_,_,_) -> ReturnVal end;
fun_from_input([_,_,_,_], ReturnVal)       -> fun(_,_,_,_) -> ReturnVal end;
fun_from_input([_,_,_], ReturnVal)         -> fun(_,_,_) -> ReturnVal end;
fun_from_input([_,_], ReturnVal)           -> fun(_,_) -> ReturnVal end;
fun_from_input(_, ReturnVal)               -> fun(_) -> ReturnVal end.
