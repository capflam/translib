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

-module(test_multiparser).
-author("christopher faulet <christopher.faulet@capflam.org>").

-include_lib("eunit/include/eunit.hrl").

-compile([{parse_transform, multiparser}]).


simple_test_() ->
    F = fun([], Res) ->
                lists:reverse(Res);
	       ([H|T], Res) ->
                Incr = _1 + 1,
                callee(T, [Incr(H)] ++ Res)
        end,
    L1 = lists:seq(1, 10),
    L2 = lists:seq(2, 11),
    [
     ?_assertEqual(L2, F(L1, []))
    ].
