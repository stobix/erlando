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
%% Copyright (c) 2011-2013 VMware, Inc.  All rights reserved.
%%

-module(monad).

-export([join/2, sequence/2]).

-type(monad(_A) :: any()). %% urm, don't know what to do here.

-compile({parse_transform, do}).

-callback '>>='(any(), fun((any()) -> any())) -> any().
-callback return(any()) -> any().
-callback fail(any()) -> any().

-spec join(atom(), monad(monad(A))) -> monad(A).
join(Monad, X) ->
    do([Monad || Y <- X,
                 Y]).
-spec sequence(atom(), [monad(A)]) -> monad([A]).
sequence(Monad, Xs) ->
    sequence(Monad, Xs, []).

sequence(Monad, [], Acc) ->
    do([Monad || return(lists:reverse(Acc))]);
sequence(Monad, [X|Xs], Acc) ->
    do([Monad || E <- X,
                 sequence(Monad, Xs, [E|Acc])]).
