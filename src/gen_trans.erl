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


%% @doc Generic parse transformer behaviour.
%%
%% A behaviour module for implementing a parse transformer of Erlang
%% code. A transformer implemented using this module must define 3
%% functions:
%%
%% <ul>
%%  <li><a href="#Module:init-1">Module:init/1</a></li>
%%  <li><a href="#Module:parse-2">Module:parse/2</a></li>
%%  <li><a href="#Module:terminate-2">Module:terminate/2</a></li>
%% </ul>
%%
%% A `gen_trans' assumes all specific parts to be located in a
%% callback module exporting a pre-defined set of functions. The
%% relationship between the behaviour functions and the callback
%% functions can be illustrated as follows:
%%
%% <div class="example">
%%```
%% gen_server module                    Callback module
%% -----------------                    ---------------
%% gen_trans:start/3      ----->        Module:init/1
%% -                      ----->        Module:parse/2
%%
%% gen_trans:parse/3      <---->        Module:parse/2
%%
%% -                      ----->        Module:terminate/2
%%'''
%% </div>
%%
%% If a callback function throw an error, the `gen_trans' will
%% terminate.
%%
%% <br/>
%% == EXPORTS ==
%%<a name="start-3"> </a>
%%```
%%start(Module, Forms, Options) -> {ok, NewForms} | {error, Errors, []}
%%'''
%% <div class="REFBODY">
%%   Types:
%%   <div class="REFTYPES">
%%     ```
%%     Module = atom()
%%     Forms, NewForms = [erlang_form()]
%%     Options = [compiler_options()] | term()
%%     Errors = [{Filename, [ErrorInfo]}]
%%        FileName = atom() | string()
%%        ErrorInfo = see separate description below.
%%     '''
%%   </div>
%%
%%   Start the transformer. The first thing this function does is to
%%   call <a href="#Module:init-1">Module:init/1</a> to initialize the
%%   internal state of the transformer.
%%
%%   `Module' is the name of the callback module. `Forms' is the
%%   Erlang abstract code format. `Options' is an arbitrary term which
%%   passed as the argument to <a
%%   href="#Module:init-1">Module:init/1</a>. If the transformer is
%%   called by the compiler, then `Options' could contain the compiler
%%   options.
%%
%%   If the abstract code format is successfully parsed, the function
%%   returns `{ok, NewForms}'. if it fails the function returns
%%   `{error, Errors, []}'.
%%
%%   See <a href="http://www.erlang.org/doc/man/compile.html">compile(3)</a>.
%%
%% </div>
%%
%%<a name="parse-3"> </a>
%%```
%%parse(Module, Form, State) -> {NewForm, NewState}
%%'''
%% <div class="REFBODY">
%%   Types:
%%   <div class="REFTYPES">
%%     ```
%%     Module = atom()
%%     Form, NewForm = erlang_form()
%%     State, NewState = term()
%%     '''
%%   </div>
%%
%%   This function is a generic parser that walk though the Erlang
%%   abstract code `Form'. At each node, it delegate the processing to
%%   `Module' by calling <a
%%   href="#Module:parse-2">Module:parse/2</a>. `State' is the
%%   internal state of the `gen_trans'.
%%
%%   Whenever a `gen_trans' is started using <a
%%   href="#start-3">gen_trans:start/3</a>, this function is called on
%%   each element of the `Forms' argument provided to the start
%%   function. Furthermore, the callback function <a
%%   href="#Module:parse-2">Module:parse/2</a> should use it to
%%   simplified the parsing.
%%
%%   See <a
%%   href="http://www.erlang.org/doc/apps/erts/absform.html">The
%%   Abstract Format</a> in ERTS User's Guide.
%%
%% </div>
%%
%% <br/>
%% == CALLBACK FUNCTIONS ==
%%
%% The following functions should be exported from a `gen_trans'
%% callback module.
%%
%% === EXPORTS ===
%%
%%<a name="Module:init-1"> </a>
%%```
%%Module:init(Options) -> {ok, State} | {stop, Reason}
%%'''
%% <div class="REFBODY">
%%   Types:
%%   <div class="REFTYPES">
%%     ```
%%     Options = [compiler_options()] | term()
%%     State = term()
%%     Reason = term()
%%     '''
%%   </div>
%%
%%   Whenever a `gen_trans' is started using <a
%%   href="#start-3">gen_trans:start/3</a>, this function is called.
%%
%%   `Options' is the `Options' argument provided to the start
%%   function.
%%
%%   If the initialization is successful, the function should return
%%   `{ok, State}', where `State' is the internal state of the
%%   `gen_trans'. If something goes wrong during the initialization
%%   the function should return `{stop, Reason}' where `Reason' is any
%%   term.
%%
%% </div>
%%
%%
%%<a name="Module:parse-2"> </a>
%%```
%%Module:parse(Form, State) -> {NewForm, NewState} |
%%                             {BeforeForm, NewForm, AfterForm, NewState}
%%'''
%% <div class="REFBODY">
%%   Types:
%%   <div class="REFTYPES">
%%     ```
%%     Form, NewForm = erlang_form()
%%     BeforeForm, AfterForm = erlang_form()
%%     State, NewState = term()
%%     '''
%%   </div>
%%
%%   At each step of the parsing, This function is called by the
%%   function <a href="#parse-3">gen_trans:parse/3</a>.
%%
%% </div>
%%
%%
%%<a name="Module:terminate-2"> </a>
%%```
%%Module:terminate(Reason, State) -> ok
%%'''
%% <div class="REFBODY">
%%   Types:
%%   <div class="REFTYPES">
%%     ```
%%     Reason = term()
%%     State = term()
%%     '''
%%   </div>
%%
%%   This function is called by a `gen_trans' when it is about to
%%   terminate. It should be the opposite of <a
%%   href="#Module:init-1">Module:init/1</a> and do any necessary
%%   cleaning up. When it returns, the `gen_trans' terminates with
%%   `Reason'. The return value is ignored.
%%
%%   `Reason' is a term denoting the stop reason and `State' is the
%%   internal state of the `gen_trans'.
%%
%%   `Reason' depends on why the `gen_trans' is terminating. If it is
%%   because another callback function has threw an error, `Reason'
%%   will have the value of the thrown error. Otherwise, the
%%   `gen_trans' terminate `Reason=normal'.
%%
%% </div>
%%
%% <br/>
%% == Error Information ==
%%
%% The `ErrorInfo' mentioned above is the standard `ErrorInfo'
%% structure which is returned from all IO modules. It has the
%% following format:
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

-module(gen_trans).
-author("christopher faulet <christopher.faulet@capflam.org>").
-version("0.1.0").

%% External exports
-export([
         behaviour_info/1,
         start/3
        ]).

%% Internal exports
-export([
         parse/3
        ]).

%%====================================================================
%% Public API
%%====================================================================
%% @hidden
behaviour_info(callbacks) ->
    [
     {init, 1},
     {terminate, 2},
     {parse, 2}
    ];
behaviour_info(_Other) ->
    undefined.


%% @hidden
start(Parser, Forms, Options) ->
    try
        State = case  Parser:init(Options) of
                    {ok, S} -> S;
                    {stop, R} -> exit({error, 0, R})
                end,

        {NewForms, NewState} = transform(Parser, Forms, State),
        Parser:terminate(normal, NewState),
        {ok, [erl_syntax:revert(T) || T <- lists:flatten(NewForms)]}
    catch
        exit:{error, Line, What} ->
            case [F || {attribute,_,file,{F,_}} <- Forms] of
                [File|_] ->
                    {error, [{File, [{Line, Parser, What}]}], []};
                [] ->
                    {error, [{command_error, [{Line, Parser, What}]}], []}
            end
    end.

%%====================================================================
%% parse_transform stuff
%%====================================================================
transform(Parser, Forms, State) ->
    transform(Parser, Forms, State, []).

%% FIXME: Add skip clause
transform(_Parser, [], NewState, NewForms) ->
    {lists:reverse(NewForms), NewState};
transform(Parser, [Form|Tail], State, Result) ->
    try Parser:parse(Form, State) of
        {NewForm, NewState} ->
            transform(Parser, Tail, NewState, [NewForm]++Result);
        {BeforeForm, NewForm, AfterForm, NewState} ->
            transform(Parser, Tail, NewState,
                      [AfterForm, NewForm, BeforeForm]++Result)
    catch
        error:function_clause ->
            {NewForm, NewState} = parse(Parser, Form, State),
            transform(Parser, Tail, NewState, [NewForm]++Result);
        error:Reason ->
            Parser:terminate(Reason, State),
            report_error(Reason, Form);
        throw:Reason ->
            Parser:terminate(Reason, State),
            report_error(Reason, Form);
        _:Error ->
            Parser:terminate(Error, State),
            exit(Error)
    end;
transform(Parser, Form, State, []) when is_tuple(Form) ->
    try
        Parser:parse(Form, State)
    catch
        error:function_clause ->
            parse(Parser, Form, State);
        error:Reason ->
            Parser:terminate(Reason, State),
            report_error(Reason, Form);
        throw:Reason ->
            Parser:terminate(Reason, State),
            report_error(Reason, Form);
        _:Error ->
            Parser:terminate(Error, State),
            exit(Error)
    end.


%%====================================================================
%% Helper functions
%%====================================================================
get_pos([]) -> 0;
get_pos(Form) -> erl_syntax:get_pos(Form).

report_error(Error, Info) ->
    exit({error, get_pos(Info), Error}).

%%====================================================================
%% Generic parser
%%====================================================================
%%
%%% Module declarations
%%

%% @hidden
parse(Parser, Forms, State) when is_list(Forms) ->
    transform(Parser, Forms, State);

%% -file(File, Line)
parse(_Parser, {attribute, _Line, file, {_File, _Line}}=Form, State) ->
    {Form, State};

%% -module(Mod)
parse(_Parser, {attribute, _Line, module, _Mod}=Form, State) ->
    {Form, State};

%% -export([Fun_1/A_1, ..., Fun_k/A_k])
parse(_Parser, {attribute, _Line, export, _Funs}=Form, State) ->
    {Form, State};

%% -import(Mod, [Fun_1/A_1, ..., Fun_k/A_k])
parse(_Parser, {attribute, _Line, import, {_Mod, _Funs}}=Form, State) ->
    {Form, State};

%% compile(Options)
parse(_Parser, {attribute, _Line, compile, _Options}=Form, State) ->
    {Form, State};

%% -record(...)
parse(Parser, {attribute, Line, record, {Name, Reps}}, State) ->
    {NewReps, NewState} = transform(Parser, Reps, State),
    NewForm = {attribute, Line, record, {Name, NewReps}},
    {NewForm, NewState};

%% -A(T)
parse(_Parser, {attribute, _Line, _A, _T}=Form, State) ->
    {Form, State};

%% function(...) when ... -> ... end
parse(Parser, {function, Line, Name, Arity, RepClauses}, State) ->
    {NewRepClauses, NewState} = transform(Parser, RepClauses, State),
    NewForm = {function, Line, Name, Arity, NewRepClauses},
    {NewForm, NewState};

%% Field A in a record
parse(Parser, {record_field, Line, RepA}, State) ->
    {NewRepA, NewState} = transform(Parser, RepA, State),
    NewForm = {record_field, Line, NewRepA},
    {NewForm, NewState};

%% Field A = E in a record || E_0#Name.Field || E_0.Field
parse(Parser, {record_field, Line, RepA, RepE}, State) ->
    {NewRepA, NewState1} = transform(Parser, RepA, State),
    {NewRepE, NewState2} = transform(Parser, RepE, NewState1),
    NewForm = {record_field, Line, NewRepA, NewRepE},
    {NewForm, NewState2};

%% E_0#Name.Field
parse(Parser, {record_field, Line, RepE0, Name, RepField}, State) ->
    {NewRepE0, NewState1} = transform(Parser, RepE0, State),
    {NewRepField, NewState2} = transform(Parser, RepField, NewState1),
    NewForm = {record_field, Line, NewRepE0, Name, NewRepField},
    {NewForm, NewState2};

%% Error
parse(_Parser, {error, _E}=Form, State) ->
    {Form, State};

%% Warning
parse(_Parser, {warning, _E}=Form, State) ->
    {Form, State};


%%
%%% Atomic literals
%%

%% integer
parse(_Parser, {integer, _Line, _I}=Form, State) ->
    {Form, State};

%% float
parse(_Parser, {float, _Line, _F}=Form, State) ->
    {Form, State};

%% string
parse(_Parser, {string, _Line, _Chars}=Form, State) ->
    {Form, State};

%% atom
parse(_Parser, {atom, _Line, _A}=Form, State) ->
    {Form, State};


%%
%%% Pattern And Expressions
%%

%% P = E_0
parse(Parser, {match, Line,  RepP, RepE0}, State) ->
    {NewRepP, NewState1} = transform(Parser, RepP, State),
    {NewRepE0, NewState2} = transform(Parser, RepE0, NewState1),
    NewForm = {match, Line,  NewRepP, NewRepE0},
    {NewForm, NewState2};

%% Universal pattern '_'
parse(_Parser, {var, _Line, '_'}=Form, State) ->
    {Form, State};

%% Variable V
parse(_Parser, {var, _Line, _V}=Form, State) ->
    {Form, State};

%% Tuple {P_1, ..., P_K}
parse(Parser, {tuple, Line, Reps}, State) ->
    {NewReps, NewState} = transform(Parser, Reps, State),
    NewForm = {tuple, Line, NewReps},
    {NewForm, NewState};

%% []
parse(_Parser, {nil, _Line}=Form, State) ->
    {Form, State};

%% [P_H | P_T]
parse(Parser, {cons, Line, RepH, RepT}, State) ->
    {NewRepH, NewState1} = transform(Parser, RepH, State),
    {NewRepT, NewState2} = transform(Parser, RepT, NewState1),
    NewForm = {cons, Line, NewRepH, NewRepT},
    {NewForm, NewState2};

%% <<P_1:Size_1/TSL_1, ..., P_K:Size_K/TSL_K>>
parse(Parser, {bin, Line, RepBins}, State) ->
    {NewRepBins, NewState} = transform(Parser, RepBins, State),
    NewForm = {bin, Line, NewRepBins},
    {NewForm, NewState};

%% binary element P:Size/TSL
parse(Parser, {bin_element, Line, RepP, RepSize, RepTSL}, State) ->
    {NewRepP, NewState} = transform(Parser, RepP, State),
    NewForm = {bin_element, Line, NewRepP, RepSize, RepTSL},
    {NewForm, NewState};

%% P_1 Op P_2
parse(Parser, {op, Line, Op, Rep1, Rep2}, State) ->
    {NewRep1, NewState1} = transform(Parser, Rep1, State),
    {NewRep2, NewState2} = transform(Parser, Rep2, NewState1),
    NewForm = {op, Line, Op, NewRep1, NewRep2},
    {NewForm, NewState2};

%% Op P_0
parse(Parser, {op, Line, Op, Rep0}, State) ->
    {NewRep0, NewState} = transform(Parser, Rep0, State),
    NewForm = {op, Line, Op, NewRep0},
    {NewForm, NewState};

%% #Name{Field_1=P_1, ..., Field_K=P_K}
parse(Parser, {record, Line, Name, RepFields}, State) ->
    {NewRepFields, NewState} = transform(Parser, RepFields, State),
    NewForm = {record, Line, Name, NewRepFields},
    {NewForm, NewState};

%% E_0#Name{Field_1=P_1, ..., Field_K=P_K}
parse(Parser, {record, Line, RepE0, Name, RepFields}, State) ->
    {NewRepE0, NewState1} = transform(Parser, RepE0, State),
    {NewRepFields, NewState2} = transform(Parser, RepFields, NewState1),
    NewForm = {record, Line, NewRepE0, Name, NewRepFields},
    {NewForm, NewState2};

%% #Name.Field_1
parse(Parser, {record_index, Line, Name, RepField}, State) ->
    {NewRepField, NewState} = transform(Parser, RepField, State),
    NewForm = {record_index, Line, Name, NewRepField},
    {NewForm, NewState};

%% E_0#Name.Field_1
parse(Parser, {record_index, Line, RepE0, Name, RepField}, State) ->
    {NewRepE0, NewState1} = transform(Parser, RepE0, State),
    {NewRepField, NewState2} = transform(Parser, RepField, NewState1),
    NewForm = {record_index, Line, NewRepE0, Name, NewRepField},
    {NewForm, NewState2};

%% catch E_0
parse(Parser, {'catch', Line, RepE0}, State) ->
    {NewRepE0, NewState} = transform(Parser, RepE0, State),
    NewForm = {'catch', Line, NewRepE0},
    {NewForm, NewState};

%% E_M:E_0(E_1, ..., E_K)
parse(Parser, {call, Line, {remote, Line1, RepEm, RepE0}, Reps}, State) ->
    {NewRepEm, NewState1} = transform(Parser, RepEm, State),
    {NewRepE0, NewState2} = transform(Parser, RepE0, NewState1),
    {NewReps, NewState3} = transform(Parser, Reps, NewState2),
    NewForm = {call, Line, {remote, Line1, NewRepEm, NewRepE0}, NewReps},
    {NewForm, NewState3};

%% E_0(E_1, ..., E_K)
parse(Parser, {call, Line, RepE0, Reps}, State) ->
    {NewRepE0, NewState1} = transform(Parser, RepE0, State),
    {NewReps, NewState2} = transform(Parser, Reps, NewState1),
    NewForm = {call, Line, NewRepE0, NewReps},
    {NewForm, NewState2};

%% [E_0 || W_1, ..., W_K]
parse(Parser, {lc, Line, RepE0, RepWs}, State) ->
    {NewRepE0, NewState1} = transform(Parser, RepE0, State),
    {NewRepWs, NewState2} = transform(Parser, RepWs, NewState1),
    NewForm =  {lc, Line, NewRepE0, NewRepWs},
    {NewForm, NewState2};

%% <<E_0 || W_1, ..., W_k>>
parse(Parser, {bc, Line, RepE0, RepWs}, State) ->
    {NewRepE0, NewState1} = transform(Parser, RepE0, State),
    {NewRepWs, NewState2} = transform(Parser, RepWs, NewState1),
    NewForm =  {bc, Line, NewRepE0, NewRepWs},
    {NewForm, NewState2};

%% begin B end
parse(Parser, {block, Line, RepB}, State) ->
    {NewRepB, NewState} = transform(Parser, RepB, State),
    NewForm = {block, Line, NewRepB},
    {NewForm, NewState};

%% if Ic_1; ...; Ic_K
parse(Parser, {'if', Line, RepIcs}, State) ->
    {NewRepIcs, NewState} = transform(Parser, RepIcs, State),
    NewForm = {'if', Line, NewRepIcs},
    {NewForm, NewState};

%% case E_0 of Cc_1 ; ...; Cc_K
parse(Parser, {'case', Line, RepE0, RepCcs}, State) ->
    {NewRepE0, NewState1} = transform(Parser, RepE0, State),
    {NewRepCcs, NewState2} = transform(Parser, RepCcs, NewState1),
    NewForm =  {'case', Line, NewRepE0, NewRepCcs},
    {NewForm, NewState2};

%% try B of Cc_1 ; ...; Cc_K catch Tc_1 ; ... ; Tc_N after A end,
parse(Parser, {'try', Line, RepB, RepCcs, RepTcs, RepA}, State) ->
    {NewRepB, NewState1} = transform(Parser, RepB, State),
    {NewRepCcs, NewState2} = transform(Parser, RepCcs, NewState1),
    {NewRepTcs, NewState3} = transform(Parser, RepTcs, NewState2),
    {NewRepA, NewState4} = transform(Parser, RepA, NewState3),
    NewForm =  {'try', Line, NewRepB, NewRepCcs, NewRepTcs, NewRepA},
    {NewForm, NewState4};

%% receive Cc_1 ; ... ; Cc_K end
parse(Parser, {'receive', Line, RepCcs}, State) ->
    {NewRepCcs, NewState} = transform(Parser, RepCcs, State),
    NewForm = {'receive', Line, NewRepCcs},
    {NewForm, NewState};

%% receive Cc_1 ; ... ; Cc_K after E_0 -> B_T end
parse(Parser, {'receive', Line, RepCcs, RepE0, RepBt}, State) ->
    {NewRepCcs, NewState1} = transform(Parser, RepCcs, State),
    {NewRepE0, NewState2} = transform(Parser, RepE0, NewState1),
    {NewRepBt, NewState3} = transform(Parser, RepBt, NewState2),
    NewForm = {'receive', Line, NewRepCcs, NewRepE0, NewRepBt},
    {NewForm, NewState3};

%% fun Name / Arity
parse(_Parser, {'fun', _Line, {function, _Name, _Arity}}=Form, State) ->
    {Form, State};

%% fun Module:Name/Arity
parse(_Parser, {'fun', _Line, {function, _Module, _Name, _Arity}}=Form, State) ->
    {Form, State};

%% fun Fc_1 ; ... ; Fc_K
parse(Parser, {'fun', Line, {clauses, RepFcs}}, State) ->
    {NewRepFcs, NewState} = transform(Parser, RepFcs, State),
    NewForm = {'fun', Line, {clauses, NewRepFcs}},
    {NewForm, NewState};

%% query [E_0 || W_1, ..., W_k]
parse(Parser, {'query', Line, {lc, Line1, RepE0, RepWs}}, State) ->
    {NewRepE0, NewState1} = transform(Parser, RepE0, State),
    {NewRepWs, NewState2} = transform(Parser, RepWs, NewState1),
    NewForm =  {'query', Line, {lc, Line1, NewRepE0, NewRepWs}},
    {NewForm, NewState2};

%% P <- E
parse(Parser, {generate, Line, RepP, RepE}, State) ->
    {NewRepP, NewState1} = transform(Parser, RepP, State),
    {NewRepE, NewState2} = transform(Parser, RepE, NewState1),
    NewForm =  {generate, Line, NewRepP, NewRepE},
    {NewForm, NewState2};

%% P <= E
parse(Parser, {b_generate, Line, RepP, RepE}, State) ->
    {NewRepP, NewState1} = transform(Parser, RepP, State),
    {NewRepE, NewState2} = transform(Parser, RepE, NewState1),
    NewForm =  {b_generate, Line, NewRepP, NewRepE},
    {NewForm, NewState2};


%%
%%% Clauses
%%
parse(Parser, {clause, Line, RepPatterns, RepGuards, RepBody}, State) ->
    {NewRepPatterns, NewState1} = transform(Parser, RepPatterns, State),
    {NewRepGuards, NewState2} = transform(Parser, RepGuards, NewState1),
    {NewRepBody, NewState3} = transform(Parser, RepBody, NewState2),
    NewForm = {clause, Line, NewRepPatterns, NewRepGuards, NewRepBody},
    {NewForm, NewState3};

%%
%%% Eof and others
%%
parse(_Parser, {eof, _Line}=Form, State) ->
    {Form, State};

parse(Parser, Forms, State) when is_list(Forms) ->
    {NewForms, NewState} = transform(Parser, Forms, State),
    {NewForms, NewState};

parse(_Parser, Form, State) ->
    {Form, State}.
