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

-module (source_ut).
-compile (export_all).
-import(testing, [use_and_purge_tree/2]).

erlang_files_test () ->
    use_and_purge_tree (
      [{file,"toto.erl",[]},
       {file, "bla.xml", []},
       {directory, "src",
	[{file,"titi.erl",[]}]}],
      fun (Dir, _) ->
	      Expected = [filename:join (Dir, X) || X <- ["src/titi.erl", "toto.erl"]],
	      Result = source:erlang_files (Dir),
	      same_elements = adlib:compare (Expected, Result)
      end).
    
module_test() ->
    Expected =
	"-module (my_module).\n"
	"-export ([foo/0]).\n"
	"foo () ->\n"
	"ok.\n",
    Expected = source:module (my_module, [{foo, ["ok."]}]).

module_file_test() ->
    Expected = {file, "my_module.erl", source:module (my_module, [{foo, ["ok."]}])},
    Expected = source:module_file (my_module, [{foo, ["ok."]}]).

lines_test() ->
    "Toto\nTiti\nTata\n" = source:lines (["Toto", "Titi", "Tata"]).
