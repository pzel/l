-module(l_tests).
-include_lib("eunit/include/eunit.hrl").

%% Basics

append_empty_to_empty_test() ->
    ?assertMatch([], l:append([], [])).

append_empty_to_singleton_test() ->
    ?assertMatch([1], l:append([], [1])).

append_two_singletons_test() ->
    ?assertMatch([1,2], l:append([1], [2])).
