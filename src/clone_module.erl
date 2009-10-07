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

%% @doc Parse transformer that builds a module by cloning the content of another
%% one.
%%
%% Performs parse transformation that makes a copy of a specified module, from
%% his beam file (by default) or his source file. This transformation uses the
%% abstract code of a module, the source module, and merges it with the abstract
%% code of another one, the destination module. It copies all attributes and
%% functions found in the abstract code of the source module, except `file' and
%% `module' attributes. For functions with same name and arity in the source
%% module and the destination module, we only keep the ones come from the
%% destination module. To deal with this goal and make the parsing easier, This
%% module also implements the {@link gen_trans} behaviour.
%%
%% To perform this kind of transformation, we must define the attribute `clone'
%% following the syntax
%%
%%     <div class="example">
%% ```
%% -clone({Module, Options}).
%% '''
%%     </div>
%%
%% where `Module' is the name of the source module and `Options' is a list of
%% options. Supported options are:
%%
%% <ul>
%%   <li>`{source, Filename}': `Filename' refers to an Erlang source file.</li>
%%   <li>`{beam, Filename}': `Filename' refers to an Erlang beam file.</li>
%% </ul>
%%
%% This is usefull to speficy the Erlang source file of a source module if we
%% cannot extract the abstract code from the beam file (See
%% [http://www.erlang.org/doc/man/beam_lib.html#debug_info beam_lib(3)]).
%%
%% {@link transhell} uses this parse transformation with the module
%% [http://www.erlang.org/doc/man/shell.html shell].
%%
%% === Examples ===
%%
%%     <div class="example">
%% ```
%% -clone({lists, []}).
%% -clone({lists, [{source, "/usr/lib/erlang/lib/stdlib-1.15.5/src/lists.erl"}]}).
%% '''
%%     </div>
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
%%   option `{parse_transform, clone_module}' is passed to the compiler. This
%%   function starts a new {@link gen_trans} by calling {@link
%%   gen_trans:start/3} with `clone_module' as callback module.
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
%       <li>`redefine_clone'. The clone attribute are redefined.</li>
%%      <li>`no_module'. The module specified to the clone attribute was not found.</li>
%%      <li>`no_abstract_code'. Abstract code chunk of the beam file is empty.</li>
%%      <li>`no_source'. The source file specified as argument to the clone attribute was not found.</li>
%%      <li>`{beam_lib_error, Error}'. `Error' returned by the module `beam_lib'.</li>
%%      <li>`{epp_error, Error}'. `Error' returned by the module `epp'.</li>
%%      <li>`{erl_lint_error, Errors}'. `Errors' returned by the module `erl_lint'.</li>
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
-module(clone_module).
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


-record(clone_infos, {local_module,
                      clone_module,
                      attribute_forms=[],
                      exports=[],
                      function_forms={[], []},
                      other_forms=[]}).

-define(ERR_REDEFINE_CLONE,   redefine_clone).
-define(ERR_MODULE_NOT_FOUND, no_module).
-define(ERR_SOURCE_NOT_FOUND, no_source).
-define(ERR_AC_EMPTY,         no_abstract_code).
-define(ERR_BEAM_LIB,         beam_lib_error).
-define(ERR_EPP,              epp_error).
-define(ERR_LINTER,           erl_lint_error).

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
format_error(badarg) ->
    "Bad argument";
format_error(?ERR_REDEFINE_CLONE) ->
    "Redefining clone attribute";
format_error(?ERR_MODULE_NOT_FOUND) ->
    "Module not found";
format_error(?ERR_SOURCE_NOT_FOUND) ->
    "Module source file not found";
format_error(?ERR_AC_EMPTY) ->
    "Abstract code chunk is empty";
format_error({?ERR_BEAM_LIB, Error}) ->
    lists:flatten(beam_lib:format_error(Error));
format_error({?ERR_EPP, Error}) ->
    lists:flatten(epp:format_error(Error));
format_error({?ERR_LINTER, [{File, ErrorInfo}]}) ->
    F = fun({Line, Module, ErrorDescriptor}, Acc) ->
                E = Module:format_error(ErrorDescriptor),
                [lists:flatten(io_lib:format("~n\t~s:~p: ~s",
                                             [File, Line, E]))|Acc]
        end,
    lists:flatten(["Errors found from module source file:",
                   lists:foldl(F, [], ErrorInfo)]);
format_error(Error) ->
    lists:flatten(io_lib:format("Unknown error: ~w", [Error])).


%%====================================================================
%% Parser API
%%====================================================================
%% @hidden
init(_Options) ->
    {ok, #clone_infos{}}.

%% @hidden
terminate(_Reason, _State) ->
    ok.

%% @hidden
%% get the module name
parse({attribute, _Line, module, Mod}=Form,
      #clone_infos{attribute_forms=Attrs}=State) ->
    {[], State#clone_infos{local_module=Mod, attribute_forms=[Form|Attrs]}};

%% Find a clone attribute. Try to get the Erlang abstract code of the given
%% module
parse({attribute, _Line, clone, {Mod, Args}}=Form,
      #clone_infos{clone_module=undefined}=State) when is_atom(Mod),
                                                       is_list(Args) ->
    AC = case proplists:get_value(source, Args) of
             undefined -> load_from_beam(proplists:get_value(beam, Args, Mod));
             SrcFile   -> load_from_source(SrcFile)
         end,
    NewState =
        lists:foldl(fun({attribute, _, file, _}, S) -> S; %% ignore it
                       ({attribute, _, module, _}, S) -> S; %% ignore it
                       ({eof, _}, S) -> S; %% ignore it
                       ({attribute, _, export, L}, S) ->
                            S#clone_infos{exports=L++S#clone_infos.exports};
                       ({attribute, _, spec, _}=Spec, S) ->
                            S#clone_infos{
                              other_forms=[Spec|S#clone_infos.other_forms]
                             };
                       ({attribute, _, _, _}=A, S) ->
                            S#clone_infos{
                              attribute_forms=[A|S#clone_infos.attribute_forms]
                             };
                       ({function, L, N, A, C}, S) ->
                            %% Parse clauses
                            {NewC, _} = parse(C, S),
                            F = {function, L, N, A, NewC},
                            {Local, Cloned} = S#clone_infos.function_forms,
                            S#clone_infos{function_forms={Local, [F|Cloned]}};
                       (F, S) ->
                            S#clone_infos{
                              other_forms=[F|S#clone_infos.other_forms]
                             }
                    end, State#clone_infos{clone_module=Mod}, AC),
    {[], NewState#clone_infos{
           clone_module=Mod,
           attribute_forms=[Form|NewState#clone_infos.attribute_forms]
          }};
parse({attribute, _, clone, {_, _}},
      #clone_infos{clone_module=_M}) when _M /= undefined ->
    erlang:error(?ERR_REDEFINE_CLONE);
parse({attribute, _, clone, {_, _}}, _State) ->
    erlang:error(badarg);

%% Merge exported functions with those of the cloned module
parse({attribute, _Line, export, Funs0}, #clone_infos{exports=Funs1}=State) ->
    {[], State#clone_infos{exports=Funs0++Funs1}};

%% Accumulate other attributes
parse({attribute, _Line, _A, _T}=Form,
      #clone_infos{attribute_forms=Attrs}=State) ->
    {[], State#clone_infos{attribute_forms=[Form|Attrs]}};

%% Accumulate function forms
parse({function, _L, _N, _A, _C}=Form,
      #clone_infos{function_forms={Local, Cloned}}=State) ->
    {[], State#clone_infos{function_forms={[Form|Local], Cloned}}};

%% Now, merge all forms
parse({eof, Line}, State) ->
    Attributes = lists:reverse(State#clone_infos.attribute_forms),
    [{attribute, ExpLine, _A, _T}|_] = State#clone_infos.attribute_forms,

    Exports = {attribute, ExpLine, export,
               lists:usort(State#clone_infos.exports)},

    {LocalFuns, ClonedFuns} = State#clone_infos.function_forms,
    FunForms = LocalFuns ++ lists:foldl(fun({function, _, Name, Arity, _},
                                            Acc) ->
                                                [F || F <- Acc,
                                                      (element(3, F) /= Name
                                                       orelse
                                                       element(4, F) /= Arity)]
                                        end, ClonedFuns, LocalFuns),

    OtherForms = State#clone_infos.other_forms,

    Forms = lists:keysort(2, Attributes) ++ [Exports] ++
        lists:keysort(2, FunForms ++ OtherForms),
    {Forms, {eof, Line}, [], State};


%% Transform remote call to the clone module in a remote call to the local
%% module
parse({call, Line, {remote, Line1, {atom, Line2, CMod}, RepE0}, Reps},
      #clone_infos{local_module=LMod, clone_module=CMod}=State) ->
    {{call, Line, {remote, Line1, {atom, Line2, LMod}, RepE0}, Reps}, State};

%% Forward other forms to the generic parser
parse(Form, State) ->
    gen_trans:parse(?MODULE, Form, State).

%%====================================================================
beam_filename(Module) when is_atom(Module) ->
    case code:where_is_file(atom_to_list(Module)++code:objfile_extension()) of
        non_existing ->
            erlang:error(?ERR_MODULE_NOT_FOUND);
        File ->
            File
    end;
beam_filename(Filename) when is_list(Filename) ->
    case filelib:is_file(Filename) of
        true  -> Filename;
        false -> erlang:error(?ERR_MODULE_NOT_FOUND)
    end.

load_from_beam(Mod) ->
    BeamFile = beam_filename(Mod),
    case beam_lib:chunks(BeamFile, [abstract_code]) of
        {ok, {Mod, [{abstract_code, {_, AC}}]}} ->
            AC;
        {ok, {Mod, [{abstract_code, no_abstract_code}]}} ->
            erlang:error(?ERR_AC_EMPTY);
        {error, beam_lib, Error} ->
            erlang:error({?ERR_BEAM_LIB, Error})
    end.

load_from_source(File) ->
    case filelib:is_file(File) of
        true  -> ok;
        false -> erlang:error(?ERR_SOURCE_NOT_FOUND)
    end,
    AC = case epp:parse_file(File, [], []) of
             {ok, Forms} -> Forms;
             {error, Error} -> erlang:error({?ERR_EPP, Error})
         end,
    case erl_lint:module(AC) of
        {ok, _} -> AC;
        {error, Errors, _} -> erlang:error({?ERR_LINTER, Errors})
    end.


%%====================================================================
