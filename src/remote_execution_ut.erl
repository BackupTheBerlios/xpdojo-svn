%%% Copyright (c) Dominic Williams, Nicolas Charpentier, Virgile Delecolle,
%%% Fabrice Nourisson, Jacques Couvreur.
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

-module (remote_execution_ut).
-export([compile_and_run_test/0]).

-import(testing, [use_and_purge_tree/2]).

file () ->
    {file,"barbapapa.erl",
     ["-module(barbapapa).",
      "-export([barbatruc/0]).",
      "barbatruc() -> casimir."]}.


compile_and_run_test () ->
    use_and_purge_tree (
      [file ()],
      fun (Dir, _) ->
	      Mock_execution_fun = fun() -> barbapapa:barbatruc () end,

	      remote_execution:start(node(), filename:join (Dir, "barbapapa.erl"), Mock_execution_fun),
	      ok = receive 
		       {_, _, casimir} ->
			   ok
		   after 1000 ->
			   timeout
		   end
      end).

