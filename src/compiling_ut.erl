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

-module(compiling_ut).
-compile(export_all).

modules_from_directory_test() ->
    [] = compiling:modules_from_directory ([], ""),
    [toto] = compiling:modules_from_directory ([{toto, "/tmp/foo/dir/toto.beam"}, {titi, "/tmp/dodo/titi.beam"}], "/tmp/foo"),
    [titi, toto] = compiling:modules_from_directory ([{toto, "/tmp/foo/dir/toto.beam"}, {tata, "/Users/foo/tata.beam"}, {titi, "/tmp/foo/titi.beam"}], "/tmp/foo").
    

differences_test() ->
    [] = compiling:differences ([], []),
    [{added, ["/tmp/titi.erl", "/tmp/toto.erl"]}] =
	compiling:differences ([], ["/tmp/toto.erl", "/tmp/titi.erl"]),
    Never_modified_fun = fun (_Module, _File) -> false end,
    [{added, ["/tmp/toto.erl"]}] =
	compiling:differences ([titi], ["/tmp/toto.erl", "/tmp/titi.erl"], Never_modified_fun),
    [] = compiling:differences ([titi, toto], ["/tmp/foo/toto.erl", "/Users/prog/titi.erl"], Never_modified_fun),
    [{deleted, [mod1, mod2]}] =
	compiling:differences ([mod1, mod4, mod2, mod3], ["/tmp/mod3.erl", "/tmp/foo/mod4.erl"], Never_modified_fun),
    [{added, ["/tmp/titi.erl"]}, {deleted, [mod1]}] =
	compiling:differences ([mod1, mod2], ["/tmp/tutu/mod2.erl", "/tmp/titi.erl"], Never_modified_fun),
    [{modified, ["/tmp/toto.erl"]}] =
	compiling:differences ([toto], ["/tmp/toto.erl"], fun (_Module, _File) -> true end),
    ok.
