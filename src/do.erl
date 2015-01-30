%% This file is a copy of erl_id_trans.erl from the R14B02 Erlang/OTP
%% distribution, with modifications to make it implement Haskell-style
%% 'do' syntax in Erlang.

%% All modifications are (C) 2011-2013 VMware, Inc; Eduard Sergeev.

%%
%% ``The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved via the world wide web at http://www.erlang.org/.
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%%
%% The Initial Developer of the Original Code is Ericsson Utvecklings AB.
%% Portions created by Ericsson are Copyright 1999, Ericsson Utvecklings
%% AB. All Rights Reserved.''
%%

-module(do).

-export([
         parse_transform/2,
         format_error/1
        ]).

parse_transform(Ast, _Options) ->
    forms(Ast).

forms([F0|Fs0]) ->
    F1 = try transform_subnodes(F0, [])
         catch throw:{Error, Line} ->
                 {error, {Line, ?MODULE, Error}}
         end,
    [F1|forms(Fs0)];
forms([]) -> [].

%%  do syntax detection:
transform_node(
    {call, Line, {atom, _Line1, do},
     [{lc, _Line2, {AtomOrVar, _Line3, _MonadModule} = Monad, Qs}]},
     MonadStack) when AtomOrVar =:= atom orelse AtomOrVar =:= var ->
    %% 'do' calls of a particular form:
    %%  do([ MonadMod || Qualifiers ])
    {call, Line,
     {'fun', Line,
      {clauses,
       [{clause, Line, [], [], do_syntax(Qs, [Monad | MonadStack])}]}}, []};
transform_node(
    {call, Line, {atom, Line1, ReturnOrFail}, As0},
    [Monad|_Monads] = MonadStack) when ReturnOrFail =:= return orelse
                                       ReturnOrFail =:= fail ->
    %% 'return' calls of a particular form:
    %%  return(Arguments), and
    %% 'fail' calls of a particular form:
    %%  fail(Arguments)
    %% Transformed to:
    %% "Monad:return(Args)" or "Monad:fail(Args)" in monadic context
    {call, Line, {remote, Line1, Monad, {atom, Line1, ReturnOrFail}},
     transform_node(As0, MonadStack)};
transform_node(Node, MonadStack) ->
    transform_subnodes(Node, MonadStack).

transform_subnodes(Node, MonadStack) when is_tuple(Node) ->
    List = tuple_to_list(Node),
    Node2 = transform_subnodes(List, MonadStack),
    list_to_tuple(Node2);
transform_subnodes(Nodes, MonadStack) when is_list(Nodes) ->
    state_map(fun transform_node/2, MonadStack, Nodes);
transform_subnodes(Node, _MonadStack) ->
    Node.

%%  'do' syntax transformation:
do_syntax([], [{_AtomOrVar, MLine, _MonadModule} | _MonadStack]) ->
    transform_error("A 'do' construct cannot be empty", MLine);
do_syntax([{GenerateOrMatch, Line, _Pattern, _Expr}], _MonadStack)
  when GenerateOrMatch =:= generate orelse GenerateOrMatch =:= match ->
    transform_error("The last statement in a 'do' construct must be an expression", Line);
do_syntax([{generate, Line, {var, _Line, _Var} = Pattern, Expr} | Exprs],
          [Monad | _Monads] = MonadStack) ->
    %% "Pattern <- Expr, Tail" where Pattern is a simple variable
    %% is transformed to
    %% "Monad:'>>='(Expr, fun (Pattern) -> Tail')"
    %% without a fail to match clause
    [{call, Line, {remote, Line, Monad, {atom, Line, '>>='}},
      [transform_node(Expr, MonadStack),
       {'fun', Line,
        {clauses,
         [{clause, Line, [Pattern], [], do_syntax(Exprs, MonadStack)}]}}]}];
do_syntax([{generate, Line, Pattern, Expr} | Exprs],
          [Monad | _Monads] = MonadStack) ->
    %% "Pattern <- Expr, Tail" where Pattern is not a simple variable
    %% is transformed to
    %% "Monad:'>>='(Expr, fun (Pattern) -> Tail')"
    %% with a fail clause if the function does not match
    [{call, Line, {remote, Line, Monad, {atom, Line, '>>='}},
      [transform_node(Expr, MonadStack),
       {'fun', Line,
        {clauses,
         [{clause, Line, [Pattern], [], do_syntax(Exprs, MonadStack)},
          {clause, Line, [{var, Line, '_'}], [],
           [{call, Line, {remote, Line, Monad, {atom, Line, 'fail'}},
             [{atom, Line, 'monad_badmatch'}]}]}]}}]}];
do_syntax([Expr], MonadStack) ->
    [transform_node(Expr, MonadStack)]; %% Don't do '>>' chaining on the last elem
do_syntax([{match, _Line, _Pattern, _Expr} = Expr | Exprs],
          MonadStack) ->
    %% Handles 'let binding' in do expression a-la Haskell
    [transform_node(Expr, MonadStack) | do_syntax(Exprs, MonadStack)];
do_syntax([Expr | Exprs], [Monad | _Monads] = MonadStack) ->
    %% "Expr, Tail" is transformed to "Monad:'>>='(Expr, fun (_) -> Tail')"
    %% Line is always the 2nd element of Expr
    Line = element(2, Expr),
    [{call, Line, {remote, Line, Monad, {atom, Line, '>>='}},
      [transform_node(Expr, MonadStack),
       {'fun', Line,
        {clauses,
         [{clause, Line,
           [{var, Line, '_'}], [], do_syntax(Exprs, MonadStack)}]}}]}].

%%  = Error formating  =========================================================

%% Use this function to report any parse_transform error. The
%% resulting error message will be displayed as an ordinary
%% compilation error in a standard format.
transform_error(Message, Line) ->
    throw({Message, Line}).

%% This function is called by the Erlang compiler to obtain an error
%% message which will be shown to the user.
format_error(Message) ->
    case io_lib:deep_char_list(Message) of
        true -> Message;
        _    -> io_lib:write(Message)
    end.

%% =============================================================================
%% Internal functions
%% =============================================================================

state_map(_Fun, _State, []) -> [];
state_map(Fun, State, [H|T]) ->
    [Fun(H, State)|state_map(Fun, State, T)].
