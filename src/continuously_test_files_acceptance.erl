%%% Copyright (c) 2005 Dominic Williams, Nicolas Charpentier, Virgile Delecolle.
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

-module(continuously_test_files_acceptance).
-compile(export_all).
-import(testing, [use_and_purge_tree/2]).

bad_foo() ->
    {file,"foo.erl",
     ["-module(foo).",
      "-export([bar/0]).",
      "baar() -> ok."]}.

foo() ->
    {file,"foo.erl",
     ["-module(foo).",
      "-export([bar/0]).",
      "bar() -> ok."]}.

bad_foo_ut() ->
    {file,"foo_ut.erl",
     ["-module(foo_ut).",
      "-export([bar_test/0]).",
      "bar_test() -> nok = foo:bar()."]}.

foo_ut() ->
    {file,"foo_ut.erl",
     ["-module(foo_ut).",
      "-export([bar_test/0]).",
      "bar_test() -> ok = foo:bar()."]}.

bad_bar() ->
    {file,"bar.erl",
     ["-module(bar).",
      "-export([foo/0]).",
      "fooo() -> ok."]}.

bar() ->
    {file,"bar.erl",
     ["-module(bar).",
      "-export([foo/0]).",
      "foo() -> ok."]}.

bad_bar_ut() ->
    {file,"bar_ut.erl",
     ["-module(bar_ut).",
      "-export([foo_test/0]).",
      "foo_test() -> nok = bar:foo()."]}.

bar_ut() ->
    {file,"bar_ut.erl",
     ["-module(bar_ut).",
      "-export([foo_test/0]).",
      "foo_test() -> ok = bar:foo()."]}.

baz() ->
    {file,"baz.erl",
     ["-module(baz).",
      "-export([run/0]).",
      "run() -> yohoho."]}.

directory_empty_test() ->
    use_and_purge_tree (
      [],
      fun(Dir,_) ->
	      no_source_files = xpdojo:test_files (Dir)
      end).

tree_without_code() ->
    [{directory, "temporary",
      [{file, "toto.txt", []},
       {file, "titi.xml", []}]},
     {directory, "temp2",
      [{file, "foo.beam", []},
       {directory,"sub",
	[{file, "truc", []},
	 {file, "machin.o", []}]}]}].
    
tree_without_code_test() ->
    use_and_purge_tree (
      tree_without_code(),
      fun (Dir,_) ->
	      no_source_files = xpdojo:test_files (Dir)
      end).

single_module_test() ->
    use_and_purge_tree (
      [foo()],
      fun (Dir,_) ->
	      [{acceptance,0,0}, {unit,0,0}, {modules,1,1}] = xpdojo:test_files (Dir)
      end).

multi_module_test() ->
    use_and_purge_tree (
      [foo(), bad_bar(), baz()],
      fun (Dir,_) ->
	      [{modules,3,2}] = xpdojo:test_files (Dir)
      end).

single_module_with_unit_test() ->
    use_and_purge_tree (
      [foo(), foo_ut()],
      fun (Dir,_) ->
	      [{acceptance,0,0}, {unit,1,1}, {modules,2,2}] = xpdojo:test_files (Dir)
      end).

multi_module_with_one_failing_unit_test() ->
    use_and_purge_tree (
      [foo(), foo_ut(), bar(), bad_bar_ut()],
      fun (Dir,_) ->
	      [{unit,2,1}, {modules,4,4}] = xpdojo:test_files (Dir)
      end).

failing_build_bails_out_test() ->
    use_and_purge_tree (
      [foo(), bad_bar(), baz(), foo_ut()],
      fun (Dir,_) ->
	      [{modules,4,3}] = xpdojo:test_files (Dir)
      end).

foo_more_utt() ->
    {file,"foo_more_utt.erl",
     ["-module(foo_more_utt).",
      "-export([bar_t/0,bar_t/1,bar_tt/0,bar2_tt/0]).",
      "bar_t() -> ok = foo:bar().",
      "bar_t(X) -> X = foo:bar().",
      "bar_tt() -> ok = foo:bar().",
      "bar2_tt() -> nok = foo:bar()."]}.

foo_utt() ->
    {file,"foo_utt.erl",
     ["-module(foo_utt).",
      "-export([bar_tt/0, toto_test/0]).",
      "bar_tt() -> ok = foo:bar().",
      "toto_test() -> ok = foo:bar()."]}.

custom_unit_filters_test() ->
    Options = 
	[{unit_modules_filter, adlib:ends_with("_utt")},
	 {unit_functions_filter, adlib:ends_with("_tt")}],
    use_and_purge_tree (
      [foo(), foo_ut(), foo_more_utt(), foo_utt()],
      fun (Dir,_) ->
	      [{unit,3,2}, {modules,4,4}] = xpdojo:test_files (Dir, Options)
      end).

foo_acceptance() ->
    {file,"foo_acceptance.erl",
     ["-module(foo_acceptance).",
      "-export([bar_tt/0, toto_test/0]).",
      "bar_tt() -> ok = foo:bar().",
      "toto_test() -> hoho = foo:bar()."]}.
    
single_failing_acceptance_test() ->
    use_and_purge_tree (
      [foo(), foo_acceptance()],
      fun (Dir,_) ->
	      [{acceptance,1,0}, {unit,0,0}, {modules,2,2}] = xpdojo:test_files (Dir)
      end).

reload_changed_file_test() ->    
    use_and_purge_tree (
      [foo(), foo_ut()],
      fun (Dir,_) ->
	      [{acceptance,0,0}, {unit,1,1}, {modules,2,2}] = xpdojo:test_files (Dir),
	      timer:sleep(2000),
	      file:write_file(filename:join(Dir,"foo_ut.erl"),
			      "-module(foo_ut).\n"
			      "-export([foo_test/0]).\n"
			      "foo_test() -> nok = foo:bar()."),
	      [{unit,1,0}, {modules,2,2}] = xpdojo:test_files (Dir)
      end).

add_module_to_directory_test() ->
    use_and_purge_tree (
      [foo(), foo_ut()],
      fun (Dir,_) ->
	      [{acceptance,0,0}, {unit,1,1}, {modules,2,2}] = xpdojo:test_files (Dir),
	      file:write_file(filename:join(Dir,"bar.erl"),
			      "-module(bar).\n"
			      "-export([foo/0]).\n"
			      "foo() -> yohoho."),
	      [{acceptance,0,0}, {unit,1,1}, {modules,3,3}] = xpdojo:test_files (Dir)
      end).

unchanged_test() ->
    use_and_purge_tree (
      [foo_acceptance(),
       {directory,"src",[foo(),bar()]},
       {directory,"unit",[foo_ut(), bar_ut()]}],
      fun (Dir,_) ->
	      [{acceptance,1,0}, {unit,2,2}, {modules,5,5}] = xpdojo:test_files (Dir),
	      unchanged = xpdojo:test_files (Dir)
      end).
    
continue_after_compile_error_test() ->
    use_and_purge_tree (
      [foo()],
      fun (Dir,_) ->
	      [{acceptance,0,0},{unit,0,0},{modules,1,1}] = xpdojo:test_files (Dir),
	      timer:sleep(2000),
	      file:write_file(filename:join(Dir,"foo.erl"),
			      "-module(foo).\n"
			      "-export([yo/0]).\n"
			      "yo() - yohoho."),
	      [{modules,1,0}] = xpdojo:test_files (Dir),
	      timer:sleep(2000),
	      file:write_file(filename:join(Dir,"foo.erl"),
			      "-module(foo).\n"
			      "-export([yo/0]).\n"
			      "yo() -> yohoho."),
	      [{acceptance,0,0},{unit,0,0},{modules,1,1}] = xpdojo:test_files (Dir)
      end).
      
custom_report_function_compile_error_test() ->
    Options = 
	[{report_function, fun my_report_function/1}],
    use_and_purge_tree (
      [bad_foo()],
      fun (Dir,_) ->
	      [{modules, 1, 0}] = xpdojo:test_files (Dir, Options),
	      Expected_filename = filename:join (Dir, "foo.erl"),
	      ok = receive
		       {compile, {error, Expected_filename, Errors, Warnings}} ->
			   ok
		   after 0 -> nok
		   end
      end).

custom_report_function_compile_success_test() ->
    Options = 
	[{report_function, fun my_report_function/1}],
    use_and_purge_tree (
      [foo()],
      fun (Dir,_) ->
	      [{acceptance, 0, 0}, {unit, 0, 0}, {modules, 1, 1}] = xpdojo:test_files (Dir, Options),
	      ok = receive
		       {compile, {ok, foo, Warnings}} ->
			   ok
		   after 0 -> nok
		   end
      end).

my_report_function ({Phase, Term}) ->
    self() ! {Phase, Term}.
