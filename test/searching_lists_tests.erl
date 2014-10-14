-module(searching_lists_tests).
-include_lib("proper_eunit/include/pt_proper_eunit.hrl").

%%
%%  Searching with a predicate
%%


%% filter/2
filter_test_() ->
    [?_assertEqual([], l:filter(fun t/1, [])),
     ?_assertEqual([], l:filter(fun f/1, []))
    ].

prop_const_true_is_id() ->
    ?FORALL(Xs, list(integer()),
            l:filter(fun t/1, Xs) == Xs).
prop_const_false_is_null() ->
    ?FORALL(Xs, list(integer()),
            l:filter(fun f/1, Xs) == []).

prop_filter_half() ->
    ?FORALL({Negs, Poss}, {list(neg_integer()), list(pos_integer())},
            begin
                All = l:append(Negs,Poss),
                FilteredPs = l:filter(fun pos/1, All),
                FilteredNs = l:filter(fun(X) -> not pos(X) end, All),

                l:length(FilteredPs) == l:length(Poss)
                andalso
                l:length(FilteredNs) == l:length(Negs)
                andalso
                l:length(l:append(FilteredNs,FilteredPs)) == l:length(All)
            end).

t(_) -> true.
f(_) -> false.
pos(N) -> N > 0.

