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

-module (compiling).
-export ([modules_from_directory/2, differences/2]).


modules_from_directory (Modules, Directory) ->
    lists:foldl (
      fun ({Module, Module_file}, Acc) ->
	      adlib:accumulate_if (Module, Acc, adlib:is_below_directory (Module_file, Directory))
      end,
      [],
      Modules).

differences (Modules, Files) ->
    differences2 (lists:sort (Modules), lists:keysort (1, add_module_key (Files)), []).

differences2 ([Module1|Remaining_modules], [{Module2,File}|Remaining_files], Acc) when Module1 == Module2 ->
    differences2 (Remaining_modules, Remaining_files, Acc);
differences2 (Modules=[Module1|Remaining_modules], [{Module2,File}|Remaining_files], Acc) when Module1 > Module2 ->
    differences2 (Modules, Remaining_files, [File|Acc]);
differences2 ([], [{Module1, File} | T], Acc) ->
    differences2 ([], T, [File|Acc]);
differences2 (_, [], []) ->
    [];
differences2 (_, [], Acc) ->
    [{added, lists:reverse (Acc)}].

add_module_key (Files) ->
    [{list_to_atom(filename:basename (X, ".erl")), X} || X <- Files].

