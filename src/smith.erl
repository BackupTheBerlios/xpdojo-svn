%%% Copyright (c) 2004-2007 Dominic Williams, Nicolas Charpentier
%%% All rights reserved.
%%% See file COPYING.

-module (smith).
-export ([start/2]).

start (File, Forge) ->
    Smith = self(),
    Module = filename:basename (File, ".erl"),
    Forge ! {Smith, File, Module, module, []},
    Assistant = spawn_link (fun () -> assistant_loop (File, Smith) end),
    Assistant ! compile,
    smith_loop(Forge, File, Module, module, Assistant).

smith_loop (Forge, File, M, Status, Assistant) ->
    receive
	{Assistant, compiled, Module, _, Warnings} ->
	    Forge ! {self(), File, Module, compiled, Warnings},
	    smith_loop (Forge, File, Module, compiled, Assistant);
	{Assistant, compile_failed, Errors, Warnings} ->
	    Forge ! {self(), File, M, compile_failed, {Errors, Warnings}},
	    smith_loop (Forge, File, M, compile_failed, Assistant);
	_ ->
	    smith_loop (Forge, File, M, Status, Assistant)
    end.

assistant_loop(File, Smith) ->
    receive
	compile ->
	    Result = compile:file (File, [debug_info, binary, return]),
	    compile_report (File, Result, Smith),
	    assistant_loop (File, Smith);
	_ ->
	    assistant_loop (File, Smith)
    end.

compile_report (File, {ok, Module, Binary, Warnings}, Smith) ->
    Smith ! {self(), compiled, Module, Binary, normalise_warnings (File, Warnings, [])};
compile_report (File, {error, Errors, Warnings}, Smith) ->
    Smith ! {self(), compile_failed, Errors, Warnings}.

normalise_warnings (File, [{File, Warnings} | Tail], Acc) ->
    normalise_warnings (File, Tail, Warnings ++ Acc);
normalise_warnings (File, [W | Ws], Acc) ->
    normalise_warnings (File, Ws, Ws ++ Acc);
normalise_warnings (_, [], Acc) ->
    Acc.
