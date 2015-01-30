%% The contents of this file are subject to the Mozilla Public License
%% Version 1.1 (the "License"); you may not use this file except in
%% compliance with the License. You may obtain a copy of the License
%% at http://www.mozilla.org/MPL/
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and
%% limitations under the License.
%%
%% The Original Code is Erlando.
%%
%% The Initial Developer of the Original Code is VMware, Inc.
%% Copyright (c) 2011-2013 VMware, Inc; Eduard Sergeev.
%% All rights reserved.
%%

-module(test_do).
-compile({parse_transform, do}).

-compile(export_all).

test_sequence() ->
    List = lists:seq(1,5),
    ListM = [do([maybe_m || return(N)]) || N <- List],
    {just, List} = monad:sequence(maybe_m, ListM).

test_join() ->
    {just, 5} = monad:join(maybe_m,
                           maybe_m:return(maybe_m:return(5))),
    {just, 5} = monad:join(maybe_m,
                           do([maybe_m || return(maybe_m:return(5))])),
    {just, 5} = monad:join(maybe_m,
                           do([maybe_m || return(do([maybe_m || return(5)]))])).

test_maybe() ->
    nothing = maybe(atom),
    {just, 9} = maybe(3).

maybe(Arg) ->
    do([maybe_m
        || monad_plus:guard(maybe_m, is_number(Arg)),
           return(Arg*Arg)]).

test_list() ->
    %% Demonstrate equivalence of list comprehensions and list monad
    A = [{X,Y} || X <- "abcd",
                  Y <- [1,2]],
    A = do([list_m || X <- "abcd",
                      Y <- [1,2],
                      return({X,Y})]),
    %% Classic pythagorean triples
    P = [{X, Y, Z} || Z <- lists:seq(1,20),
                      X <- lists:seq(1,Z),
                      Y <- lists:seq(X,Z),
                      math:pow(X,2) + math:pow(Y,2) == math:pow(Z,2)],
    P = do([list_m || Z <- lists:seq(1,20),
                      X <- lists:seq(1,Z),
                      Y <- lists:seq(X,Z),
                      monad_plus:guard(
                        list_m, math:pow(X,2) + math:pow(Y,2) == math:pow(Z,2)),
                      return({X,Y,Z})]).

test_omega() ->
    A = [{X,Y,Z} || X <- "abcd",
                    Y <- lists:seq(1,5),
                    Z <- lists:seq(11,15)],
    B = do([omega_m || X <- "abcd",
                       Y <- lists:seq(1,5),
                       Z <- lists:seq(11,15),
                       return({X,Y,Z})]),
    true = A =/= B,
    true = A =:= lists:usort(B).

%% Tests for 'let-match binding' (a-la 'let' in Haskell's 'do'
%% expression) But instead of 'let' here we use 'match' (=) expression
%% in 'do([])':
test_let_match() ->
    T1 = do([maybe_m || R <- return(2),
                        R2 = R*R,
                        return(R2*R2)]),
    T1 = do([maybe_m || R <- return(2),
                        return(R*R*R*R)]),
    %% Failure test
    T2 = do([error_m || A <- return(42),
                        {B,C} <- fail(test),
                        BC = B*C,
                        return(BC+A)]),
    T2 = do([error_m || A <- return(42),
                        {B,C} <- fail(test),
                        return(B*C+A)]),

    Fun = fun({X,Y}) -> {Y,X} end, %% Mysterious function
    T3 = do([error_m || R <- return({1,42}),
                        {R1,R2} = Fun(R),
                        return(R1+R2)]),
    T3 = do([error_m || R <- return({1,42}),
                        %% No better way without 'let'?
                        %% Well, only via extra 'return'
                        return(element(1,Fun(R)) + element(2,Fun(R)))]),

    DivRem = fun(N,M) -> {N div M,N rem M} end,
    T4 = do([error_m || {N,M} <- return({42,3}),
                        {D,R} = DivRem(N,M),
                        E <- T3,
                        S = D+R+E,
                        return({D,R,S})]),
    T4 = do([error_m || {N,M} <- return({42,3}),
                        %% Can hack it with extra 'return' (and '>>='
                        %% as result)
                        {D,R} <- return(DivRem(N,M)),
                        E <- T3,
                        return({D,R,D+R+E})]),

    T5 = do([list_m || X <- [1,2,3],
                       X2 = X*X,
                       Y <- lists:seq(1,X2),
                       Y2 = {Y,X2},
                       Z = Y + X2,
                       return({X2,Y,Y2,Z})]),
    T5 = do([list_m || X <- [1,2,3],
                       Y <- lists:seq(1,X*X),
                       return({X*X,Y,{Y,X*X},Y+X*X})]).

test_let_first() ->
    M = do([list_m || A = 3,
                      X <- [1,2,A],
                      Y <- [A,A+1],
                      return({X,Y})]),
    M = fun() ->
                A = 3,
                do([list_m || X <- [1,2,A],
                              Y <- [A,A+1],
                              return({X,Y})])
        end().

test_let_escapes() ->
    M1 = do([maybe_m || A = 5,
                        return(A)]),
    M2 = do([maybe_m || A = 6,
                        return(A)]),
    M1 = do([maybe_m || return(5)]),
    M2 = do([maybe_m || return(6)]),

    %% Demonstrate that bindings do not escape.
    M3 = do([maybe_m || return(_A = 5)]),
    M3 = do([maybe_m || return((_A = 7) - 2)]),
    _A = 6.

test() ->
    test:test([{?MODULE, [test_sequence,
                          test_join,
                          test_maybe,
                          test_list,
                          test_omega,
                          test_let_match,
                          test_let_first,
                          test_let_escapes]}],
              [report, {name, ?MODULE}]).
