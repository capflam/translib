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

%% @doc Module that clones the module [http://www.erlang.org/doc/man/shell.html
%% shell] using the parse transformer {@link clone_module}.
%%
%% Provides the same API that the module
%% [http://www.erlang.org/doc/man/shell.html shell] and performs exactly the
%% same treatements except that before evaluating expressions, this module parse
%% transforms these. To do so, it calls the parse transformer {@link
%% multiparser}. The configuration parameter `shell_parse_transform' for
%% application `translib' can be used to personnalized the parse transformers
%% chain. By default, it uses {@link recfun} and {@link lambda_expr}.
%%
%%
%% === Examples ===
%%
%%     <div class="example">
%% ```
%% Erlang (BEAM) emulator version 5.6.5 [source] [smp:2] [async-threads:0] [kernel-poll:false]
%%
%% Eshell V5.6.5  (abort with ^G)
%% 1> transhell:start().
%% <0.35.0>
%% Eshell V5.6.5  (abort with ^G)
%% [transhell:1]> application:set_env(translib, shell_parse_transform, [recfun, lambda_expr]). %% Same that the default value
%% ok
%% [transhell:2]> Adder = _1 + _2.
%% #Fun<erl_eval.12.113037538>
%% [transhell:3]> Adder(5,10).
%% 15
%% [transhell:4]> lists:map(lambda(Adder(_1, 1)), [1,2,3]).
%% [2,3,4]
%% [transhell:5]> Fact = fun(1) -> 1; (X) -> X * callee(X-1) end.
%% #Fun<erl_eval.6.13229925>
%% [transhell:6]> Fact(5).
%% 120
%% [transhell:7]>
%% '''
%%     </div>
%%

-module(transhell).

-compile([{parse_transform, clone_module}]).

-clone({shell, [{source, {stdlib, "src/shell.erl"}}]}).


-define(DEF_PARSE_TRANSFORM, [recfun, lambda_expr]).


parse_command(Prompt) ->
    Res = io:parse_erl_exprs(Prompt),
    case Res of
        {ok, ExprList0, EndLine} ->
            ParserList =
                case  application:get_env(translib, shell_parse_transform) of
                    {ok, L} when is_list(L) -> L;
                    _ -> ?DEF_PARSE_TRANSFORM
                end,
            case multiparser:parse_transform(ExprList0,
                                             [{chain, ParserList}]) of
                {error, [{command_error, [{Line, Parser, What}]}], _Warnings} ->
                    {error, {Line, Parser, What}, EndLine};
                Exprlist1 ->
                    {ok, Exprlist1, EndLine}
            end;
        _ ->
            Res
    end.

get_command(Prompt, Eval, Bs, RT, Ds) ->
    application:start(translib),
    Parse = fun() -> exit(parse_command(Prompt)) end,
    Pid = spawn_link(Parse),
    get_command1(Pid, Eval, Bs, RT, Ds).

prompt(N) ->
    case is_alive() of
        true -> {format,<<"(~s)[transhell:~w]> ">>,[node(),N]};
        false -> {format,<<"[transhell:~w]> ">>,[N]}
    end.
