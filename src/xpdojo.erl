%%% Copyright (c) Dominic Williams, Nicolas Charpentier.
%%% All rights reserved.
%%% 
%%% Redistribution and use in source and binary forms, with or without
%%% modification, are permitted provided that the following conditions are
%%% met:
%%% 
%%% * Redistributions of source code must retain the above copyright
%%%   notice, this list of conditions and the following disclaimer.
%%% * Redistributions in binary form must reproduce the above copyright
%%%   notice, this list of conditions and the following disclaimer in the
%%%   documentation and/or other materials provided with the distribution.
%%% * The names of the authors may not be used to endorse or promote
%%%   products derived from this software without specific prior written
%%%   permission.
%%% 
%%% THIS SOFTWARE IS PROVIDED BY THE AUTHORS "AS IS" AND ANY EXPRESS OR
%%% IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
%%% WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
%%% DISCLAIMED. IN NO EVENT SHALL THE AUTHORS BE LIABLE FOR ANY DIRECT,
%%% INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
%%% (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
%%% SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
%%% HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
%%% STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING
%%% IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
%%% POSSIBILITY OF SUCH DAMAGE.

-module(xpdojo).

-export([test_files/1, test_files/2, default_options/0]).

default_options() ->
    [{unit_modules_filter, adlib:ends_with("ut")},
     {unit_functions_filter, adlib:ends_with("test")}].

test_files (Directory) ->
    test_files(Directory, default_options()).

test_files(Directory, Options) ->
    Unit = unit (Options),
    with (Directory,
	  [fun find_modules/2,
	   fun find_differences/2,
	   fun (_Dir2, Files) -> acceptance(Unit(compile(Files))) end],
	  []).

with (Directory, [Fun|T], Acc) ->
    case Fun (Directory, Acc) of
	{stop, Result} ->
	    Result;
	New_acc ->
	    with (Directory, T, New_acc)
    end;
with (_Directory, [], Acc) ->
    Acc.

find_modules (Directory, _) ->
    case source:erlang_files (Directory) of
	[] ->
	    {stop, no_source_files};
	Files ->
	    Files
    end.

find_differences (Directory, Files) ->
    Loaded_modules = compiling:loaded_modules (Directory),
    case compiling:differences (Loaded_modules, Files) of
	[] ->
	    {stop, unchanged};
	_Changes ->
	    Files
    end.

compile (Files) ->
    lists:foldl (
      fun (File, {Total, Modules}) ->
	      {Total + 1, load_and_accumulate_if_succeeded (compile:file (File), Modules)}
      end,
      {0, []},
      Files).

load_and_accumulate_if_succeeded ({ok, Module}, Acc) ->
    code:purge(Module),
    {module, Module} = code:load_file(Module),
    [Module|Acc];
load_and_accumulate_if_succeeded (_, Acc) ->
    Acc.

unit (Options) ->
    {value, {_,  Module_filter}} = lists:keysearch (unit_modules_filter, 1, Options),
    {value, {_,  Function_filter}} = lists:keysearch (unit_functions_filter, 1, Options),
    unit (Module_filter, Function_filter).

unit (Mod_filter, Fun_filter) ->
    fun({Total_module_count, Compiled_modules}) when length(Compiled_modules) < Total_module_count; Total_module_count == 0 ->
	    [{modules, Total_module_count, Compiled_modules}];
       ({Total_module_count, Compiled_modules}) ->
	    test_pass(Mod_filter, Fun_filter, Compiled_modules, unit, [{modules, Total_module_count, Compiled_modules}])
    end.

acceptance ([{modules, Count, Compiled_modules}]) ->
    [{modules, Count, length (Compiled_modules)}];
acceptance ([{unit, Total, Successes}, {modules, Count, Compiled_modules}]) when Successes < Total ->
    [{unit, Total, Successes}, {modules, Count, length (Compiled_modules)}];
acceptance ([{unit, UnitTotal, UnitSuccesses}, {modules, Module_total, Compiled_modules}]) ->
    test_pass (adlib:ends_with("_acceptance"), adlib:ends_with("_test"), Compiled_modules, acceptance,
	       [{unit, UnitTotal, UnitSuccesses}, {modules,Module_total,length(Compiled_modules)}]).
    
test_pass (Module_filter, Function_filter, Modules, PassName, Acc) ->
    Tests = [X || X <- Modules, Module_filter(X)],
    {Total,Failures} = testing:run_modules (Tests, Function_filter),
    report(Failures),
    [{PassName, Total, Total- length (Failures)} | Acc ].
    
report([]) ->
    ok;
report([H|T]) ->
    io:fwrite("~p~n",[H]),
    report(T).
