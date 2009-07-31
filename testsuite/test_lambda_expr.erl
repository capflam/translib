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

-module(test_lambda_expr).
-author("christopher faulet <christopher.faulet@capflam.org>").

-include_lib("eunit/include/eunit.hrl").

-compile([{parse_transform, lambda_expr}]).

add(A, B) ->
    A + B.

twice(A) ->
    2 * A.

partial_application_test_() ->
    Adder = fun(A,B) ->
                    A + B
            end,
    Incr1 = Adder(_1, 1),
    Incr2 = add(_1, 1),
    [
     ?_assertEqual(Incr1(2), Adder(2, 1)),
     ?_assertEqual(Incr2(2), add(2, 1))
    ].

composition_test_() ->
    Adder = fun(A,B) ->
                    A + B
            end,
    Twicer = fun(A) ->
                     2 * A
             end,
    F1 = Twicer(Adder(_1, _2)),
    F2 = twice(add(_1, _2)),
    [
     ?_assertEqual(F1(1, 2), Twicer(Adder(1, 2))),
     ?_assertEqual(F2(1, 2), twice(add(1, 2)))
    ].

unary_op_test_() ->
    F = -add(_1, _2),
    [
     ?_assertEqual((-_1)(1), -1),
     ?_assertEqual(F(1, 2), -add(1, 2))
    ].


binary_op_test_() ->
    [
     ?_assertEqual((_1 + _2)(1, 2), 1 + 2)
    ].

lambda_test_() ->
    [
     ?_assertEqual(lists:map(lambda(_1 + 1), [1,2,3]), [2,3,4])
    ].
