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

-module (source).
-export ([erlang_files/1, module/2, module_file/2, lines/1]).

erlang_files (Directory) ->
    adlib:fold_files (
      Directory,
      fun ([regular,".erl",Name],Acc) ->
	      [Name|Acc];
	  (_,Acc) ->
	      Acc
      end,
      [type, extension, absolute_full_name],
      []).

module (Module, [{Function, Lines}]) ->
    lines (
      ["-module (" ++ atom_to_list (Module) ++ ").",
       "-export ([" ++ atom_to_list (Function) ++ "/0]).",
       atom_to_list (Function) ++ " () ->"
       | Lines]);
module (Module, [Function]) ->
    module (Module, [{Function, ["ok."]}]).

module_file (Module, Functions) ->
    {file, atom_to_list(Module) ++ ".erl", module (Module, Functions)}.
lines (Lines) ->
    lines (Lines, []).

lines ([Line | T ], Acc) ->
    lines (T, [$\n, Line | Acc]);
lines ([], Acc) ->
    lists:flatten (lists:reverse (Acc)).
    
	
