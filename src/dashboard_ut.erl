%%% Copyright (c) 2004-2007 Dominic Williams, Nicolas Charpentier
%%% All rights reserved.
%%% See file COPYING.

-module(dashboard_ut).
-compile(export_all).

new_test() ->
    {dashboard, {{compiled, 0, 0, 0}, {unit, 0, 0, 0}, {acceptance, 0, 0, 0}}} = dashboard:new ().

update_test() ->
    lists:foldl (
      fun ({Event, Expected}, {Count, Initital}) ->
	      {Count, {dashboard, Expected}} = {Count, dashboard:update (Event, {dashboard, Initital})},
	      {Count + 1, Expected}
      end,
      {1,         {{compiled, 0, 0, 0}, {unit, 0, 0, 0}, {acceptance, 0, 0, 0}}},
      [{module,   {{compiled, 0, 0, 1}, {unit, 0, 0, 0}, {acceptance, 0, 0, 0}}},
       {compiled, {{compiled, 1, 0, 1}, {unit, 0, 0, 0}, {acceptance, 0, 0, 0}}}]).

