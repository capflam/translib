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

-module(test_recfun).
-author("christopher faulet <christopher.faulet@capflam.org>").

-include_lib("eunit/include/eunit.hrl").

-compile([{parse_transform, recfun}]).


simple_test_() ->
    ListLength = fun([], N) ->
                         N;
                    ([_|T], N) ->
                         callee(T, N+1)
                 end,
    L = lists:seq(1, 10),
    [
     ?_assertEqual(length(L), ListLength(L, 0)),
     ?_assertEqual(length(L), (fun([], N) ->
                                       N;
                                  ([_|T], N) ->
                                       callee(T, N+1)
                               end)(L, 0))
    ].


nested_recfun_test_() ->
    ListLength1 = fun(L) ->
                          NestedFun = fun([], N) ->
                                              N;
                                         ([_|T], N) ->
                                              callee(T, N+1)
                                      end,
                          NestedFun(L, 0)
                  end,
    ListLength2 = fun([], N) ->
                          N;
                     ([_|T], N) ->
                          Incr = fun(0, J) -> J+1;
                                    (I, J) -> callee(I-1, J+1)
                                 end,
                          callee(T, Incr(N, 0))
                  end, 
    L = lists:seq(1, 10),
    [
     ?_assertEqual(length(L), ListLength1(L)),
     ?_assertEqual(length(L), ListLength2(L, 0))
    ].

param_recfun_test_() ->
    Fun = fun(F, L) ->
                  F(L, 0)
          end,
    L = lists:seq(1, 10),
    [
     ?_assertEqual(length(L), Fun(fun([], N) -> N;
                                     ([_|T], N) -> callee(T, N+1)
                                  end,
                                  L))
    ].
