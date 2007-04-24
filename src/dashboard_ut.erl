%%% Copyright (c) 2004-2007 Dominic Williams, Nicolas Charpentier
%%% All rights reserved.
%%% See file COPYING.

-module(dashboard_ut).
-compile(export_all).

new_test() ->
    {dashboard, {{modules, 0, 0, 0}, {unit, 0, 0, 0}, {acceptance, 0, 0, 0}}} = dashboard:new ().

update_test() ->
    lists:foldl (
      fun ({Event, Expected}, {Count, Initital}) ->
	      {Count, {dashboard, Expected}} = {Count, dashboard:update (Event, {dashboard, Initital})},
	      {Count + 1, Expected}
      end,
      {1,               {{modules, 0, 0, 0}, {unit, 0, 0, 0}, {acceptance, 0, 0, 0}}},
      [{{unknown,              {module, uncompiled}}, {{modules, 0, 0, 1}, {unit, 0, 0, 0}, {acceptance, 0, 0, 0}}},
       {{{module, uncompiled}, {module, errors}},     {{modules, 0, 1, 1}, {unit, 0, 0, 0}, {acceptance, 0, 0, 0}}},
       {{{module, errors},     {module, uncompiled}}, {{modules, 0, 0, 1}, {unit, 0, 0, 0}, {acceptance, 0, 0, 0}}},
       {{{module, uncompiled}, {module, compiled}},   {{modules, 1, 0, 1}, {unit, 0, 0, 0}, {acceptance, 0, 0, 0}}},
       {{unknown,              {module, uncompiled}}, {{modules, 1, 0, 2}, {unit, 0, 0, 0}, {acceptance, 0, 0, 0}}},
       {{{module, uncompiled}, {module, errors}},     {{modules, 1, 1, 2}, {unit, 0, 0, 0}, {acceptance, 0, 0, 0}}},
       {{{module, compiled},   {module, uncompiled}}, {{modules, 0, 1, 2}, {unit, 0, 0, 0}, {acceptance, 0, 0, 0}}},
       {{unknown,              {module, uncompiled}}, {{modules, 0, 1, 3}, {unit, 0, 0, 0}, {acceptance, 0, 0, 0}}},
       {{{module, uncompiled}, {module, compiled}},   {{modules, 1, 1, 3}, {unit, 0, 0, 0}, {acceptance, 0, 0, 0}}},
       {{{module, compiled},   {unit,   compiled}},   {{modules, 1, 1, 3}, {unit, 0, 0, 1}, {acceptance, 0, 0, 0}}},
       {{{module, uncompiled}, {module, compiled}},   {{modules, 2, 1, 3}, {unit, 0, 0, 1}, {acceptance, 0, 0, 0}}},
       {{{module, errors},     {module, uncompiled}}, {{modules, 2, 0, 3}, {unit, 0, 0, 1}, {acceptance, 0, 0, 0}}},
       {{{module, uncompiled}, {module, compiled}},   {{modules, 3, 0, 3}, {unit, 0, 0, 1}, {acceptance, 0, 0, 0}}}]).

%%% module, unit, acceptance
%%% uncompiled, errors, compiled, failed, passed
