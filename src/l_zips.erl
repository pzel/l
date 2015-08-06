-module(l_zips).
-export([tup/2, tup/3, tup/4]).
%% Implement when needed
%% -export([tup/5, tup/6, tup/7]).

-spec tup(A,B) -> {A,B}.
tup(A,B) -> {A,B}.

-spec tup(A,B,C) -> {A,B,C}.
tup(A,B,C) -> {A,B,C}.

-spec tup(A,B,C,D) -> {A,B,C,D}.
tup(A,B,C,D) -> {A,B,C,D}.

%% -spec tup(A,B,C,D,E) -> {A,B,C,D,E}.
%% tup(A,B,C,D,E) -> {A,B,C,D,E}.

%% -spec tup(A,B,C,D,E,F) -> {A,B,C,D,E,F}.
%% tup(A,B,C,D,E,F) -> {A,B,C,D,E,F}.

%% -spec tup(A,B,C,D,E,F,G) -> {A,B,C,D,E,F,G}.
%% tup(A,B,C,D,E,F,G) -> {A,B,C,D,E,F,G}.
