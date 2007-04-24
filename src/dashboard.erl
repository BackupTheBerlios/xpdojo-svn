%%% Copyright (c) 2004-2007 Dominic Williams, Nicolas Charpentier
%%% All rights reserved.
%%% See file COPYING.

-module (dashboard).
-export ([new/0, update/2]).
    
new () ->
    {dashboard, {{modules, 0, 0, 0}, {unit, 0, 0, 0}, {acceptance, 0, 0, 0}}}.

update ({unknown, {module, uncompiled}}, {dashboard, {{modules, Yes, No, Total}, Unit, Acceptance}}) ->
    {dashboard, {{modules, Yes, No, Total + 1}, Unit, Acceptance}};
update ({{module, uncompiled}, {module, errors}}, {dashboard, {{modules, Yes, No, Total}, Unit, Acceptance}}) ->
    {dashboard, {{modules, Yes, No + 1, Total}, Unit, Acceptance}};
update ({{module, errors}, {module, uncompiled}}, {dashboard, {{modules, Yes, No, Total}, Unit, Acceptance}}) ->
    {dashboard, {{modules, Yes, No - 1, Total}, Unit, Acceptance}};
update ({{module, uncompiled}, {module, compiled}}, {dashboard, {{modules, Yes, No, Total}, Unit, Acceptance}}) ->
    {dashboard, {{modules, Yes + 1, No, Total}, Unit, Acceptance}};
update ({{module, compiled}, {module, uncompiled}}, {dashboard, {{modules, Yes, No, Total}, Unit, Acceptance}}) ->
    {dashboard, {{modules, Yes - 1, No, Total}, Unit, Acceptance}};
update ({{module, compiled}, {unit, compiled}}, {dashboard, {Modules, {unit, Yes, No, Total}, Acceptance}}) ->
    {dashboard, {Modules, {unit, Yes, No, Total + 1}, Acceptance}}.
