%%% Copyright (c) 2004-2007 Dominic Williams, Nicolas Charpentier
%%% All rights reserved.
%%% See file COPYING.

-module (smith).
-export ([start/2]).

start (File, Forge) ->
    Smith = self(),
    Module = filename:basename (File, ".erl"),
    Forge ! {Smith, File, Module, module},
    Assistant = spawn_link (fun () -> assistant_loop (File, Smith) end),
    Assistant ! compile,
    smith_loop(Forge, File, Module, module, Assistant).

smith_loop (Forge, File, M, Status, Assistant) ->
    receive
	{Assistant, compiled, Module, _, _} ->
	    Forge ! {self(), File, Module, compiled},
	    smith_loop (Forge, File, Module, compiled, Assistant);
	_ ->
	    smith_loop (Forge, File, M, Status, Assistant)
    end.

assistant_loop(File, Smith) ->
    receive
	compile ->
	    Result = compile:file (File, [debug_info, binary, return]),
	    compile_report (Result, Smith),
	    assistant_loop (File, Smith);
	_ ->
	    assistant_loop (File, Smith)
    end.

compile_report ({ok, Module, Binary, Warnings}, Smith) ->
    Smith ! {self(), compiled, Module, Binary, Warnings}.
