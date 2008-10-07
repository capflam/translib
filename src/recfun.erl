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

%% @doc Parse_transform that makes possible to easily write recursive
%% anonymous function.
%%
%% This module implements the parse_transform that modifies the
%% semantic of Erlang to simplify the writing of recursive anonymous
%% functions. To deal with this goal and make the parsing easier, This
%% module also implements the {@link gen_trans} behaviour.
%%
%% To perform this kind of transformation, we reserved the keyword
%% `callee' that refers to the anonymous function itself. this keyword
%% plays the role of a special function name with, obviously, the same
%% number of arguments that the anonymous function referred
%% to. Furthermore, to deal with anonymous functions nested in other
%% ones, we hijacked the notation of a Mnesia record access inside a
%% query to recursively call parent functions. So, the notation
%% `(parent.)+callee' refers to parent anonymous functions.
%%
%% === Examples ===
%%
%% <ul> 
%%   <li> A simple example: the factorial function. This recursive
%%   anonymous function:
%%     <div class="example">
%% ```
%% Fact = fun(0) -> 1;
%%           (N) -> N * callee(N-1)
%%        end,
%% Fact(5). %% return 5! = 120
%% '''
%%     </div>
%%
%% is transformed by the module `recfun' in
%%
%%     <div class="example">
%% ```
%% Fact = (fun(Callee) ->
%%                fun(0) -> 1;
%%                   (N) -> N * (Callee(Callee))(N-1)
%%                end
%%         end)(fun(Callee) ->
%%                      fun(0) -> 1;
%%                         (N) -> N * (Callee(Callee))(N-1)
%%                      end
%%              end),
%% Fact(5). %% return 5! = 120
%% '''
%%     </div>
%%   </li>
%%   <li> A trickier one: This recursive anonymous function:
%%     <div class="example">
%% ```
%% Fact = fun(N) ->
%%              G = fun(0) -> 1;
%%                     (M) -> M * parent.callee(M-1)
%%                  end,
%%              G(N)
%%        end,
%% Fact(5). %% return 5! = 120
%% '''
%%     </div>
%%
%% is transformed by the module `recfun' in
%%
%%     <div class="example">
%% ```
%% Fact = (fun(Callee) ->
%%                 fun(N) ->
%%                         G = fun(0) -> 1;
%%                                (M) -> M * (Callee(Callee))(M-1)
%%                             end,
%%                         G(N)
%%                 end
%%         end)(fun(Callee) ->
%%                      fun(N) ->
%%                              G = fun(0) -> 1;
%%                                     (M) -> M * (Callee(Callee))(M-1)
%%                                  end,
%%                              G(N)
%%                      end
%%              end),
%% Fact(5). %% return 5! = 120
%% '''
%%     </div>
%%   </li>
%% </ul>
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
%%     Options = [compiler_options()]
%%     Errors = [{Filename, [ErrorInfo]}]
%%        FileName = atom() | string()
%%        ErrorInfo = see separate description below.
%%     '''
%%   </div>
%%
%%   Implements the actual transformation at compile time. This
%%   function is called by the compiler to do the source code
%%   transformation if and when the option `{parse_transform, recfun}'
%%   is passed to the compiler. This function starts a new {@link
%%   gen_trans} by calling {@link gen_trans:start/3} with `recfun' as
%%   callback module.
%%
%%   If the abstract code format is successfully parsed, the function
%%   returns `{ok, NewForms}'. if it fails the function returns
%%   `{error, Errors, []}'.
%%
%%   See <a href="http://www.erlang.org/doc/man/compile.html">compile(3)</a>.
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
%%   Takes an error code returned by one of the other functions in the
%%   module and creates a textual description of the error. `ErrCode'
%%   can be either of the following terms. The `format_error/1'
%%   function returns a descriptive string which describes the error.
%%
%%   <ul>
%%      <li>`reserved_callee_usage'. Atom `callee': reserved syntax.</li>
%%      <li>`recfun_badrec'. Bad recursive call.</li>
%%      <li>`recfun_badarity'. Bad recursive function arity.</li>
%%      <li>Other `ErrCode'. Unknown error: `ErrCode'.</li>
%%   </ul>
%%
%% </div>
%%
%%
%% <br/>
%% == Error Information ==
%%
%% The `ErrorInfo' mentioned above is the standard `ErrorInfo'
%% structure which is returned from `recfun' module. It has the
%% following format:
%%
%% <div class="example">
%%```{ErrorLine, recfun, ErrorDescriptor}'''
%% </div>
%%
%% A string which describes the error is obtained with the following
%% call:
%%
%% <div class="example">
%%```apply(recfun, format_error, ErrorDescriptor)'''
%% </div>
%%

-module(recfun).
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

-record(recfun_infos, {fun_stack=[]}).
-record(fun_infos, {ref, arity, line, is_recursive=false}).

-define(ERR_BADARITY,    recfun_badarity).
-define(ERR_BADREC,      recfun_badrec).
-define(ERR_CALLEE_NAME, reserved_callee_usage).

-define(VAR_BASENAME, "Callee_").


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
format_error(?ERR_BADARITY) ->
    "Bad recursive function arity";
format_error(?ERR_BADREC) ->
    "Bad recursive call";
format_error(?ERR_CALLEE_NAME) ->
    "Atom 'callee': reserved syntax";
format_error(Error) ->
    lists:flatten(io_lib:format("Unknown error: ~w", [Error])).


%%====================================================================
%% Parser API
%%====================================================================
%% @hidden
init(_Options) ->
    {ok, #recfun_infos{}}.

%% @hidden
terminate(_Reason, _State) ->
    ok.

%% @hidden
parse({call, Line, {atom, Line, callee}, RepArgs},
      #recfun_infos{fun_stack=Stack}=State) ->
    case Stack of
        [FunInfos|Rest] ->
            case length(RepArgs) of
                N when N =:= FunInfos#fun_infos.arity ->
                    NewStack = [FunInfos#fun_infos{is_recursive=true}] ++ Rest,
                    TmpState = State#recfun_infos{fun_stack=NewStack},
                    
                    {NewRepArgs, NewState} = parse(RepArgs, TmpState),

                    NewForm = make_recursive_call(FunInfos#fun_infos.ref, 
                                                  Line, 
                                                  NewRepArgs),
                    
                    {NewForm, NewState};
                _ ->
                    erlang:error(?ERR_BADARITY)
            end;
        [] ->
            erlang:error(?ERR_CALLEE_NAME)
    end;

parse({call, Line, {record_field, Line, RepP, {atom, Line, callee}}, RepArgs},
      #recfun_infos{fun_stack=Stack}=State) ->
    case is_recursive_call(RepP) of
        {true, Deep} when Deep =< length(Stack) ->
            FunInfos = lists:nth(Deep, Stack),            
            case length(RepArgs) of
                N when N =:= FunInfos#fun_infos.arity ->
                    NewStack = lists:keyreplace(
                                 FunInfos#fun_infos.ref, 2, 
                                 Stack,
                                 FunInfos#fun_infos{is_recursive=true}
                                ),
                    TmpState = State#recfun_infos{fun_stack=NewStack},
                    
                    {NewRepArgs, NewState} = parse(RepArgs, TmpState),
                    
                    NewForm = make_recursive_call(FunInfos#fun_infos.ref, Line,
                                                  NewRepArgs),
                    {NewForm, NewState};
                _ ->
                    erlang:error(?ERR_BADARITY)
            end;
        {true, _Deep} ->
            erlang:error(?ERR_BADREC);
        false ->
            erlang:error(?ERR_CALLEE_NAME)
    end;

parse({'fun', Line, {clauses, RepFcs}}=Form, State) ->
    FunInfos = #fun_infos{ref=make_ref(),
                          arity=erl_syntax:fun_expr_arity(Form),
                          line=Line,
                          is_recursive=false},
    Stack = [FunInfos] ++ State#recfun_infos.fun_stack,
    {NewRepFcs, TmpState} = parse(RepFcs, State#recfun_infos{fun_stack=Stack}),
    
    NewStack = TmpState#recfun_infos.fun_stack,
    NewFunInfos = hd(NewStack),
    NewForm = 
        case NewFunInfos#fun_infos.is_recursive of
            true ->
                make_recursive_fun(NewFunInfos#fun_infos.ref, Line, NewRepFcs);
            false ->
                syntax_tree(fun_expr, Line, [NewRepFcs])
        end,
    NewState = TmpState#recfun_infos{fun_stack=tl(NewStack)},
    {NewForm, NewState};

parse({atom, _Line, callee}, _State) ->
    erlang:error(?ERR_CALLEE_NAME);

%% Forward other forms to the generic parser
parse(Form, State) ->
    gen_trans:parse(?MODULE, Form, State).


%%====================================================================
%% Test if an expression matches the following pattern:
%% 'parent[.parent]*'. If yes, the parent expression is a recusrive
%% call.
is_recursive_call(RepP) ->
    is_recursive_call(RepP, 1).

is_recursive_call({atom, _Line, parent}, Deep) ->
    {true, Deep+1};
is_recursive_call({atom, _Line, _Name}, _Deep) ->
    false;
is_recursive_call({record_field, _Line, RepP, {atom, _Line, parent}}, Deep) ->
    is_recursive_call(RepP, Deep+1);
is_recursive_call(_, _Deep) ->
    false.


make_recursive_call(Ref, Pos, Args) ->
    Var = syntax_tree(variable, Pos, [make_var(Ref)]),
    Callee = syntax_tree(application, Pos, [Var, [Var]]),
    syntax_tree(application, Pos, [Callee, Args]).

make_recursive_fun(Ref, Pos, Clauses) ->
    Var = syntax_tree(variable, Pos, [make_var(Ref)]),
    C = syntax_tree(clause, Pos, 
                    [[Var], [], [syntax_tree(fun_expr, Pos, [Clauses])]]),
    F = syntax_tree(fun_expr, Pos, [[C]]),
    syntax_tree(application, Pos, [F, [F]]).
 
make_var(Ref) ->
    list_to_atom(?VAR_BASENAME ++ erlang:ref_to_list(Ref)).


%% Helper function to build an erl_parse-compatible representation of
%% a syntax tree
syntax_tree(Fun, Pos, Args) ->
    SyntaxTree = apply(erl_syntax, Fun, Args),
    erl_syntax:revert(erl_syntax:set_pos(SyntaxTree, Pos)).


