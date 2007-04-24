%%% Copyright (c) 2004-2007 Dominic Williams, Nicolas Charpentier
%%% All rights reserved.
%%% See file COPYING.

-module (smith).
-export ([start/2]).

start (File, Forge) ->
    Smith = self(),
    Module = filename:basename (File, ".erl"),
    Forge ! {Smith, File, Module, {unknown, {module, uncompiled}}, []},
    Assistant = spawn_link (fun () -> assistant_loop (File, Smith) end),
    Assistant ! compile,
    smith_loop(Forge, File, Module, {module, uncompiled}, Assistant).

smith_loop (Forge, File, M, {Type, _}=Status, Assistant) ->
    receive
	{Assistant, compiled, Module, _, Warnings} ->
	    New_status = {Type, compiled},
	    Forge ! {self(), File, Module, {Status, New_status}, Warnings},
	    smith_loop (Forge, File, Module, New_status, Assistant);
	{Assistant, compile_failed, Errors, Warnings} ->
	    New_status = {Type, errors},
	    Forge ! {self(), File, M, {Status, New_status}, {Errors, Warnings}},
	    smith_loop (Forge, File, M, New_status, Assistant);
	{Forge, modified} ->
	    New_status = {Type, uncompiled},
	    Forge ! {self(), File, M, {Status, New_status}, []},
	    Assistant ! compile,
	    smith_loop (Forge, File, M, New_status, Assistant);
	_ ->
	    smith_loop (Forge, File, M, Status, Assistant)
    end.

assistant_loop(File, Smith) ->
    receive
	compile ->
	    Result = compile:file (File, [debug_info, binary, warn_unused_import, return]),
	    compile_report (Result, Smith),
	    assistant_loop (File, Smith);
	_ ->
	    assistant_loop (File, Smith)
    end.

compile_report ({ok, Module, Binary, Warnings}, Smith) ->
    Smith ! {self (), compiled, Module, Binary, Warnings};
compile_report ({error, Errors, Warnings}, Smith) ->
    Smith ! {self (), compile_failed, Errors, Warnings}.

%% normalise_warnings (Warnings) ->
%%     lists:foldl (
%%       fun ({File, Warning}, Acc) -> orddict:append_list (File, Warning, Acc) end,
%%       orddict:new (),
%%       Warnings).
