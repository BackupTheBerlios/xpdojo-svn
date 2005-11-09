%%% Copyright (c) Dominic Williams
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

-module (directory_tree_ut).
-compile (export_all).

-include_lib ("kernel/include/file.hrl").

changes_test () ->
    {MegaSeconds, Seconds, MicroSeconds} = T0 = erlang:now (),
    T1 = {MegaSeconds, Seconds, MicroSeconds + 1},
    [] =
	lists:foldl(
	  fun({Tree1, Tree2, Expected}, Acc) ->
		  case directory_tree:changes (Tree1, Tree2) of
		      Expected ->
			  Acc;
		      Other ->
			  [{found,Other,expected,Expected}|Acc]
		  end
	  end,
	  [],
	  [{[], [], []},
	   {[{"toto.txt", T0}], [{"toto.txt", T0}], []},
	   {[{"toto.txt", T0}], [{"toto.txt", T1}], [{modified, ["toto.txt"]}]},
	   {[{"toto.txt", T0}, {"titi.xml", T1}], [{"toto.txt", T1}, {"titi.xml", T1}], [{modified, ["toto.txt"]}]},
	   {[], [{"foo.erl", T0}], [{found, ["foo.erl"]}]},
	   {[{"toto.txt", T1}], [{"toto.txt", T1}, {"foo.erl", T0}], [{found, ["foo.erl"]}]},
	   {[{"toto.txt", T1}], [{"foo.erl", T0}, {"toto.txt", T1}], [{found, ["foo.erl"]}]}
	  ]).


