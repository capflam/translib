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

%% @doc Parse_transform that can perform several parse_transformations
%%
%% This module implements the parse_transform that can perform many
%% successive transformations. Each transformation takes place in
%% module that implements the {@link gen_trans} behaviour.  The
%% parse_tranformers chain is defined as an element of the compiler
%% options list and follows the format `{chain, [ModuleName]}'.
%%
%%
%% <br/>
%% == EXPORTS ==
%%<a name="parse_transform-2"> </a>
%%```
%%parse_transform(Forms, Options) -> {ok, NewForms} | {error, Errors, []}
%%'''
%% <div class="REFBODY">
%%   Types:
%%   <div class="REFTYPES">
%%     ```
%%     Forms, NewForms = [erlang_form()]
%%     Options = [compiler_options()] ++ [{chain, [ModuleName]}]
%%        ModuleName = atom()
%%     Errors = [{Filename, [ErrorInfo]}]
%%        FileName = atom() | string()
%%        ErrorInfo = see separate description below.
%%     '''
%%   </div>
%%
%%   Implements the actual transformation at compile time. This
%%   function is called by the compiler to do the source code
%%   transformation if and when the option `{parse_transform,
%%   multiparser}' is passed to the compiler.
%%
%%   This function loops through the list `[ModuleName]' and calls
%%   {@link gen_trans:start/3} with `ModuleName' as callback
%%   module. The Erlang abstract code format in entry of an iteration
%%   is the result of the previous one.
%%
%%   If the abstract code format is successfully parsed, the function
%%   returns `{ok, NewForms}'. if it fails the function returns
%%   `{error, Errors, []}'.
%%
%%   See <a href="http://www.erlang.org/doc/man/compile.html">compile(3)</a>.
%%
%% </div>
%%
%% <br/>
%% == Error Information ==
%%
%% The `ErrorInfo' mentioned above is the standard `ErrorInfo'
%% structure which is returned from one of the chained modules. It has
%% the following format:
%%
%% <div class="example">
%%```{ErrorLine, Module, ErrorDescriptor}'''
%% </div>
%%
%% A string which describes the error is obtained with the following
%% call:
%%
%% <div class="example">
%%```apply(Module, format_error, ErrorDescriptor)'''
%% </div>
%%

-module(multiparser).
-author("christopher faulet <christopher.faulet@capflam.org>").

-export([
         parse_transform/2
        ]).

%% @hidden
parse_transform(Forms, Options) ->
    Chain = proplists:get_value(chain, Options, []),
    chain_parser(Chain, Forms, Options).

chain_parser([], Forms, _Options) ->
    Forms;
chain_parser([Module|Chain], Forms, Options) -> 
    case gen_trans:start(Module, Forms, Options) of
        {ok, NewForms} ->
            chain_parser(Chain, NewForms, Options);
        {error, EList, WList} ->
            {error, EList, WList}
    end.
