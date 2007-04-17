%%% Copyright (c) 2004-2007 Dominic Williams, Nicolas Charpentier
%%% All rights reserved.
%%% See file COPYING.

-module (dashboard).
-export ([new/0, update/2]).
    
new () ->
    {dashboard, {{compiled, 0, 0, 0}, {unit, 0, 0, 0}, {acceptance, 0, 0, 0}}}.

update (module, {dashboard, {{compiled, Yes, No, Total}, Unit, Acceptance}}) ->
    {dashboard, {{compiled, Yes, No, Total + 1}, Unit, Acceptance}};
update (compiled, {dashboard, {{compiled, Yes, No, Total}, Unit, Acceptance}}) ->
    {dashboard, {{compiled, Yes + 1, No, Total}, Unit, Acceptance}};
update (compile_failed, {dashboard, {{compiled, Yes, No, Total}, Unit, Acceptance}}) ->
    {dashboard, {{compiled, Yes, No + 1, Total}, Unit, Acceptance}}.
