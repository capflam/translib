%% =====================================================================
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
%%     $Id$
%% =====================================================================
%%
%% @author Christopher Faulet <christopher.faulet@capflam.org>
%%   [http://www.capflam.org]
%% @version {@vsn} - {@date}
%% @end

%% @doc Parse transformer that simplifies the partial application and the
%% composistion of functions in Erlang.
%%
%% Performs parse transformion that modifies the semantic of Erlang to simplify
%% the writing of partial application and composistion of functions
%% expressions. To deal with this goal and make the parsing easier, This module
%% also implements the {@link gen_trans} behaviour.
%%
%%
%%
%% <br/>
%% == EXPORTS ==
%%<a name="parse_transform-2"> </a>
%%```
%%parse_transform(Forms, Options) -> NewForms | {error, Errors, []}
%%'''
%% <div class="REFBODY">
%%   Types:
%%   <div class="REFTYPES">
%%     ```
%%     Forms, NewForms = [erlang_form()]
%%     Options = [compiler_options()]
%%     Errors = [{Filename, [ErrorInfo]}]
%%        FileName = atom() | string()
%%        ErrorInfo = see separate description below.
%%     '''
%%   </div>
%%
%%   Implements the actual transformation at compile time. This function is
%%   called by the compiler to do the source code transformation if and when the
%%   option `{parse_transform, lambda_expr}' is passed to the compiler. This
%%   function starts a new {@link gen_trans} by calling {@link
%%   gen_trans:start/3} with `lambda_expr' as callback module.
%%
%%   If the abstract code format is successfully parsed, the function returns
%%   `NewForms'. if it fails the function returns `{error, Errors, []}'.
%%
%%   See [http://www.erlang.org/doc/man/compile.html compile(3)].
%%
%% </div>
%%
%%<a name="format_error-1"> </a>
%%```
%%format_error(ErrCode) -> ErrMessage
%%'''
%% <div class="REFBODY">
%%   Types:
%%   <div class="REFTYPES">
%%     ```
%%     Errcode = term()
%%     ErrMessage = string()
%%     '''
%%   </div>
%%
%%   Takes an error code returned by one of the other functions in the module
%%   and creates a textual description of the error. The `ErrCode' can be either
%%   of the following terms. The `format_error/1' function returns a descriptive
%%   string which describes the error.
%%
%%   <ul>
%%      <li>`lambda_expr_badarg'. Argument of lambda/1 is not a lambda expression.</li>
%%      <li>`{lambda_expr_badarity, N}'. lambda/1 called with `N' arguments. (`N' /= 1) </li>
%%      <li>`{reserved_placeholder_usage, Var}'. Variable `Var': syntax reserved for placeholders.</li>
%%      <li>`invalid_placeholder_number'. Placeholder _0 is illegal.</li>
%%      <li>Other `ErrCode'. Unknown error: `ErrCode'.</li>
%%   </ul>
%%
%% </div>
%%
%%
%% <br/>
%% == Error Information ==
%%
%% The `ErrorInfo' mentioned above is the standard `ErrorInfo' structure which
%% is returned from `lambda_expr' module. It has the following format:
%%
%% <div class="example">
%%```{ErrorLine, lambda_expr, ErrorDescriptor}'''
%% </div>
%%
%% A string which describes the error is obtained with the following call:
%%
%% <div class="example">
%%```apply(lambda_expr, format_error, ErrorDescriptor)'''
%% </div>
%%

-module(lambda_expr).
-author("christopher faulet <christopher.faulet@capflam.org>").

-behaviour(gen_trans).

%% API
-export([
         parse_transform/2,
         format_error/1
        ]).

%% gen_trans callbacks
-export([
         init/1,
         terminate/2,
         parse/2
        ]).


-define(ERR_LAMBDA_FUN_ARG,     lambda_expr_badarg).
-define(ERR_LAMBDA_FUN_ARITY,   lambda_expr_badarity).
-define(ERR_PLACEHOLDERS_NAME,  reserved_placeholder_usage).
-define(ERR_PLACEHOLDERS_ZERO,  invalid_placeholder_number).

-record(lambda_infos, {in_lambda_expr = false}).


%%====================================================================
%% parse_transform API
%%====================================================================
%% @hidden
parse_transform(Forms, Options) ->
    case gen_trans:start(?MODULE, Forms, Options) of
        {ok, NewForms} ->
            NewForms;
        {error, EList, WList} ->
            {error, EList, WList}
    end.


%% @hidden
format_error(?ERR_LAMBDA_FUN_ARG) ->
    "Argument of lambda/1 is not a lambda expression";
format_error({?ERR_LAMBDA_FUN_ARITY, 0}) ->
    "lambda/1 called with no arguments";
format_error({?ERR_LAMBDA_FUN_ARITY, A}) ->
    lists:flatten(io_lib:format("lambda/1 called with ~p arguments", [A]));
format_error({?ERR_PLACEHOLDERS_NAME, V}) ->
    lists:flatten(
      io_lib:format("Variable ~w: syntax reserved for placeholders", [V])
     );
format_error(?ERR_PLACEHOLDERS_ZERO) ->
    "Placeholder _0 is illegal";
format_error(Error) ->
    lists:flatten(io_lib:format("Unknown error: ~w", [Error])).


%%====================================================================
%% gen_trans API
%%====================================================================

%% @hidden
init(_Options) ->
    {ok, #lambda_infos{}}.

%% @hidden
terminate(_Reason, _State) ->
    ok.


%% @hidden
%% Lambda call: encapsulate a lambda expression to not propagate it to
%% the parent expression
parse({call, Line, {atom, Line, lambda}, [Expr]}, _State) ->
    %% Check if Expr is a lambda expression or not
    case is_lambda_expr([Expr]) of
        true ->
            {LambdaExpr, _NewState} =
                parse(Expr, #lambda_infos{in_lambda_expr=false}),
            {LambdaExpr, #lambda_infos{}};
        false ->
            erlang:error(?ERR_LAMBDA_FUN_ARG)
    end;
parse({call, Line, {atom, Line, lambda}, [_|_]=Exprs}, _State) ->
    erlang:error({?ERR_LAMBDA_FUN_ARITY, length(Exprs)});
parse({call, Line, {atom, Line, lambda}, []}, _State) ->
    erlang:error({?ERR_LAMBDA_FUN_ARITY, 0});



%% Other calls: see if it is a lambda expression and build the
%% corresponding function
parse({call, Line, RepE0, Reps}=Form, #lambda_infos{in_lambda_expr=false}) ->
    case is_lambda_expr(Reps) of
        true ->
            {NewRepE0, _NewState} =
                parse(RepE0, #lambda_infos{in_lambda_expr=false}),

            Pattern = lambda_expr_pattern(Line, Reps),

            %% Search some inner lambda expressions
            NewReps = lists:map(
                        fun(Rep) ->
                                {NewRep, _} =
                                    parse(Rep,
                                          #lambda_infos{in_lambda_expr=true}),
                                NewRep
                        end, Reps
                       ),

            %% Build the syntax tree of the lambda expression
            Call = syntax_tree(application, Line, [NewRepE0, NewReps]),
            Clause = syntax_tree(clause, Line, [Pattern, [], [Call]]),
            NewForm = syntax_tree(fun_expr, Line, [[Clause]]),
            {NewForm, #lambda_infos{}};
        false ->
            gen_trans:parse(?MODULE, Form, #lambda_infos{})
    end;


%% See if binary operation is a lambda expression and build the
%% corresponding function
parse({op, Line, Op, P1, P2}=Form, #lambda_infos{in_lambda_expr=false}) ->
    case is_lambda_expr([P1, P2]) of
        true ->
            Pattern = lambda_expr_pattern(Line, [P1, P2]),

            %% Search some inner lambda expressions
            {NewP1, _} = parse(P1, #lambda_infos{in_lambda_expr=true}),
            {NewP2, _} = parse(P2, #lambda_infos{in_lambda_expr=true}),

            %% Build the syntax tree of the lambda expression
            InfixOp = syntax_tree(infix_expr, Line,
                                  [NewP1, erl_syntax:operator(Op), NewP2]),
            Clause = syntax_tree(clause, Line, [Pattern, [], [InfixOp]]),
            NewForm = syntax_tree(fun_expr, Line, [[Clause]]),
            {NewForm, #lambda_infos{}};
        false ->
            gen_trans:parse(?MODULE, Form, #lambda_infos{})
    end;

%% See if unary operation is a lambda expression and build the
%% corresponding function
parse({op, Line, Op, P}=Form, #lambda_infos{in_lambda_expr=false}) ->
    case is_lambda_expr([P]) of
        true ->
            Pattern = lambda_expr_pattern(Line, [P]),

            %% Search some inner lambda expressions
            {NewP, _} = parse(P, #lambda_infos{in_lambda_expr=true}),

            %% Build the syntax tree of the lambda expression
            PrefixOp = syntax_tree(prefix_expr, Line,
                                   [erl_syntax:operator(Op), NewP]),
            Clause = syntax_tree(clause, Line, [Pattern, [], [PrefixOp]]),
            NewForm = syntax_tree(fun_expr, Line, [[Clause]]),
            {NewForm, #lambda_infos{}};
        false ->
            gen_trans:parse(?MODULE, Form, #lambda_infos{})
    end;


parse({var, _, V}=Form, #lambda_infos{in_lambda_expr=false}=State) ->
    case is_placeholder(Form) of
        true ->
            erlang:error({?ERR_PLACEHOLDERS_NAME, V});
        false ->
            gen_trans:parse(?MODULE, Form, State)
    end;

%% Forward other forms to the generic parser
parse(Form, State) ->
    gen_trans:parse(?MODULE, Form, State).

%%====================================================================
%% Test if a list of expressions contains any placeholders. If yes, the
%% parent expression is a lambda expression
is_lambda_expr([]) ->
    false;
is_lambda_expr([{var, Line, V}|T]) ->
    case is_placeholder({var, Line, V}) of
        true ->
            %% We found a placeholder, we stop the recursion and
            %% return true
            true;
        false ->
            is_lambda_expr(T)
    end;
is_lambda_expr([{call, _Line, {atom, _Line, lambda}, _Reps}|T]) ->
    is_lambda_expr(T);
is_lambda_expr([{call, _Line, _RepE0, Reps}|T]) ->
    is_lambda_expr(Reps) orelse is_lambda_expr(T);
is_lambda_expr([{op, _Line, _Op, P1, P2}|T]) ->
    is_lambda_expr([P1,P2]) orelse is_lambda_expr(T);
is_lambda_expr([{op, _Line, _Op, P}|T]) ->
    is_lambda_expr([P]) orelse is_lambda_expr(T);
is_lambda_expr([_H|T]) ->
    is_lambda_expr(T).


%% Search placeholders in a list of expressions to build the list of
%% patterns of the lambda expression
lambda_expr_pattern(Line, Reps) ->
    N = get_max_placeholder(Reps, 0),
    case N  > 0 of
        true ->
            lists:map(fun(X) ->
                              V = list_to_atom("_" ++ integer_to_list(X)),
                              {var, Line, V}
                      end, lists:seq(1, N));
        false ->
            erlang:error(?ERR_PLACEHOLDERS_ZERO)
    end.

get_max_placeholder([], N) ->
    N;
get_max_placeholder([{var, Line, V}|T], N) ->
    case is_placeholder({var, Line, V}) of
        true ->
            get_max_placeholder(T, max(N, placeholder_value(V)));
        false ->
            get_max_placeholder(T, N)
    end;
get_max_placeholder([{call, _Line, {atom, _Line, lambda}, _Reps}|T], N) ->
    get_max_placeholder(T, N);
get_max_placeholder([{call, _Line, _RepE0, Reps}|T], N) ->
    M = get_max_placeholder(Reps, N),
    get_max_placeholder(T, max(N, M));
get_max_placeholder([{op, _Line, _Op, P1, P2}|T], N) ->
    M = get_max_placeholder([P1,P2], N),
    get_max_placeholder(T, max(N, M));
get_max_placeholder([{op, _Line, _Op, P}|T], N) ->
    M = get_max_placeholder([P], N),
    get_max_placeholder(T, max(N, M));
get_max_placeholder([_H|T], N) ->
    get_max_placeholder(T, N).

%% A placeholder is a variable following this pattern: '_NN', where NN
%% is any integer.
is_placeholder({var, _L, '_0'}) ->
    erlang:error(?ERR_PLACEHOLDERS_ZERO);
is_placeholder({var, _L, V}) ->
    S = atom_to_list(V),
    (hd(S) =:= $_) andalso is_integer(catch list_to_integer(tl(S))).

%% Return the integer part of placeholders
placeholder_value(V) ->
    list_to_integer(tl(atom_to_list(V))).

%% Helper finction to get max value between 2 integers
max(A, B) when A >= B -> A;
max(_A, B) -> B.

%% Helper function to build an erl_parse-compatible representation of
%% a syntax tree
syntax_tree(Fun, Pos, Args) ->
    SyntaxTree = apply(erl_syntax, Fun, Args),
    erl_syntax:revert(erl_syntax:set_pos(SyntaxTree, Pos)).


