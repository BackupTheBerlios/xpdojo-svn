%%% Copyright (c) 2004 Dominic Williams, Nicolas Charpentier.
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

-module(zero_button_test_files_cat).
-compile(export_all).

empty_list_cat()->
    Project = [{files,[]}],
    empty_project = xpdojo:dashboard(Project, unit_tests).

nonexistent_file_cat() ->
    Project = [{files,[adlib:temporary_pathname()]}],
    empty_project = xpdojo:dashboard(Project, unit_tests).

incorrect_file_cat() ->
    Dir = adlib:temporary_pathname(),
    Tree = [{file,"foo.erl",
	     ["-module(foo).",
	      "-export([bar/0]).",
	      "baar() -> ok."]}],
    adlib:make_tree(Dir,Tree),
    Project = [{files,[filename:join(Dir,"foo.erl")]}],
    build_failed = xpdojo:dashboard(Project, unit_tests),
    adlib:delete_tree(Dir).

no_tests_cat() ->		 
    Dir = adlib:temporary_pathname(),
    Tree = [{file,"foo.erl",
	     ["-module(foo).",
	      "-export([bar/0]).",
	      "bar() -> ok."]}],
    adlib:make_tree(Dir,Tree),
    Project = [{files,[filename:join(Dir,"foo.erl")]}],
    {0,[]} = xpdojo:dashboard(Project, unit_tests),
    adlib:delete_tree(Dir).

single_module_cat() ->
    Dir = adlib:temporary_pathname(),
    Tree = [{file,"foo.erl",
	     ["-module(foo).",
	      "-export([bar/0]).",
	      "bar() -> ok."]},
	    {file,"foo_ut.erl",
	     ["-module(foo_ut).",
	      "-export([bar_test/0]).",
	      "bar_test() -> nok = foo:bar()."]}],
    adlib:make_tree(Dir,Tree),
    Project = [{files,[filename:join(Dir,"foo.erl")]}],
    {1,[_Reason]} = xpdojo:dashboard(Project, unit_tests),
    adlib:delete_tree(Dir).

