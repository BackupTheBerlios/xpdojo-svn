%%% Copyright (c) 2004-2005 Dominic Williams, Nicolas Charpentier,
%%% Fabrice Nourisson, Jacques Couvreur, Virgile Delecolle.
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

foo() ->
    {file,"foo.erl",
     ["-module(foo).",
      "-export([bar/0]).",
      "bar() -> ok."]}.

unique_erlang_module() ->
    {file,"uniqueModule.erl",
     ["-module(uniqueModule).",
      "-export([bar/0]).",
      "bar() -> ok."]}.

unique_erlang_module_beam() ->
    "uniqueModule.beam".

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

silent_report(_) ->
    ok.

options() ->
    [{report_function, fun silent_report/1}, {slave_name, test_slave}].

wait() ->
    timer:sleep(1000).

directory_empty_test() ->
    use_and_purge_tree (
      [],
      fun(Dir,_) ->
              no_source_files = xpdojo:test_files (Dir, options())
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
              no_source_files = xpdojo:test_files (Dir, options())
      end).

single_module_test() ->
    use_and_purge_tree (
      [foo()],
      fun (Dir,_) ->
              [{acceptance,0,0}, {unit,0,0}, {modules,1,1}] = xpdojo:test_files (Dir, options())
      end).

multi_module_test() ->
    use_and_purge_tree (
      [foo(), bad_bar(), baz()],
      fun (Dir,_) ->
              [{modules,3,2}] = xpdojo:test_files (Dir, options())
      end).

single_module_with_unit_test() ->
    use_and_purge_tree (
      [foo(), foo_ut()],
      fun (Dir,_) ->
              [{acceptance,0,0}, {unit,1,1}, {modules,2,2}] = xpdojo:test_files (Dir, options())
      end).

multi_module_with_one_failing_unit_test() ->
    use_and_purge_tree (
      [foo(), foo_ut(), bar(), bad_bar_ut()],
      fun (Dir,_) ->
              [{unit,2,1}, {modules,4,4}] = xpdojo:test_files (Dir, options())
      end).

failing_build_bails_out_test() ->
    use_and_purge_tree (
      [foo(), bad_bar(), baz(), foo_ut()],
      fun (Dir,_) ->
              [{modules,4,3}] = xpdojo:test_files (Dir, options())
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
         {unit_functions_filter, adlib:ends_with("_tt")},
         {report_function, fun silent_report/1},
	 {slave_name, test_slave}],
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

one_acceptance_ok() ->
    {file,"one_acceptance.erl",
     ["-module(one_acceptance).",
      "-export([good_test/0]).",
      "good_test() -> ok = ok ."]}.
    
single_failing_acceptance_test() ->
    use_and_purge_tree (
      [foo(), foo_acceptance()],
      fun (Dir,_) ->
              [{acceptance,1,0}, {unit,0,0}, {modules,2,2}] = xpdojo:test_files (Dir, options())
      end).

reload_changed_file_test() ->    
    use_and_purge_tree (
      [foo(), foo_ut()],
      fun (Dir,_) ->
              [{acceptance,0,0}, {unit,1,1}, {modules,2,2}] = xpdojo:test_files (Dir, options()),
              wait(),
              file:write_file(filename:join(Dir,"foo_ut.erl"),
                              "-module(foo_ut).\n"
                              "-export([foo_test/0]).\n"
                              "foo_test() -> nok = foo:bar()."),
              [{unit,1,0}, {modules,2,2}] = xpdojo:test_files (Dir, options())
      end).

add_module_to_directory_test() ->
    use_and_purge_tree (
      [foo(), foo_ut()],
      fun (Dir,_) ->
              [{acceptance,0,0}, {unit,1,1}, {modules,2,2}] = xpdojo:test_files (Dir, options()),
              file:write_file(filename:join(Dir,"bar.erl"),
                              "-module(bar).\n"
                              "-export([foo/0]).\n"
                              "foo() -> yohoho."),
              [{acceptance,0,0}, {unit,1,1}, {modules,3,3}] = xpdojo:test_files (Dir, options())
      end).

continuous_tester_test() ->
    use_and_purge_tree (
      [foo(), foo_ut()],
      fun (Dir,_) ->
	      Silly_server_writer = fun(Silly_key) ->
					    file:write_file(
					      filename:join(Dir, "silly.erl"),
					      source:module(
						silly, [{loop,
							 ["receive",
							  "{Pid, key} ->",
							  "Pid ! " ++ Silly_key ++ ",",
							  "loop();",
							  "hotupgrade ->",
							  "?MODULE:loop()",
							  "end."]}]))
				    end,
	      Silly_server_writer("charpi"),
	      {compile, {ok, silly}} = {compile, compile:file(filename:join(Dir,"silly"), [{outdir, Dir}])},
	      {load, {module, silly}} = {load, code:load_abs(filename:join(Dir, "silly"))},
	      Silly = spawn(silly, loop, []),
	      
              Key = now(),
              Self = self(),
              Notification = fun ( Message ) ->
                                     Self ! {Key,Message}
                             end,

	      lists:foldl(
		fun({Action, Expected_message}, Index) ->
			Action(),
			{Index, Expected_message} = {Index, receive Message -> Message after 1000 -> timeout end},
			Index + 1
		end,
		1,
		[{fun() -> Silly ! {Self, key} end,
		  charpi},
		 {fun() -> testing_server:start (Dir, Notification, options()) end,
		  {Key, [{acceptance, 0, 0}, {unit, 1, 1}, {modules, 3, 3}]}},
		 {fun() -> timer:sleep(1000),Silly_server_writer("domi") end,
		  {Key, [{acceptance, 0, 0}, {unit, 1, 1}, {modules, 3, 3}]}},
		 {fun() -> Silly ! {Self, key} end,
		  charpi},
		 {fun() -> file:write_file (filename:join (Dir, "bar.erl"), source:module (bar, [foo])) end,
		  {Key, [{acceptance, 0, 0}, {unit, 1, 1}, {modules, 4, 4}]}},
		 {fun() -> testing_server:stop() end,
		  timeout},
		 {fun() -> file:write_file (filename:join (Dir, "pepe.erl"), source:module (pepe, [juan])) end,
		  timeout}])
      end).

continue_after_compile_error_test() ->
    use_and_purge_tree (
      [foo()],
      fun (Dir,_) ->
              [{acceptance,0,0},{unit,0,0},{modules,1,1}] = xpdojo:test_files (Dir, options()),
              wait(),
              file:write_file(filename:join(Dir,"foo.erl"),
                              "-module(foo).\n"
                              "-export([yo/0]).\n"
                              "yo() - yohoho."),
              [{modules,1,0}] = xpdojo:test_files (Dir, options()),
              wait(),
              file:write_file(filename:join(Dir,"foo.erl"),
                              "-module(foo).\n"
                              "-export([yo/0]).\n"
                              "yo() -> yohoho."),
              [{acceptance,0,0},{unit,0,0},{modules,1,1}] = xpdojo:test_files (Dir, options())
      end).


custom_report_function_compile_error_test() ->
    Options = 
        [{report_function, fun my_report_function/1},
	 {slave_name, test_slave}],
    use_and_purge_tree (
      [source:module_file(foo, [{bar, ["does not compile"]}])],
      fun (Dir,_) ->
              [{modules, 1, 0}] = xpdojo:test_files (Dir, Options),
              Expected_filename = filename:join (Dir, "foo.erl"),
              {ok, Errors} =
                  receive
                      {compile, {error, Expected_filename, Errors2, _Warnings}} ->
                          {ok, Errors2}
                  after 0 -> message_not_found
                  end,
              true = length(Errors) > 0
      end).

custom_report_function_compile_success_test() ->
    Options = 
        [{report_function, fun my_report_function/1},
	 {slave_name, test_slave}],
    use_and_purge_tree (
      [foo()],
      fun (Dir,_) ->
              [{acceptance, 0, 0}, {unit, 0, 0}, {modules, 1, 1}] = xpdojo:test_files (Dir, Options),
              Expected_filename = filename:join (Dir, "foo.erl"),
              ok = receive
                       {compile, {ok, Expected_filename, _}} ->
                           ok;
                       Message ->
                           Message
                   after 0 -> message_not_found
                   end
      end).

custom_report_function_unit_error_test() ->
    Options = 
        [{report_function, fun my_report_function/1},
	 {slave_name, test_slave}],
    use_and_purge_tree (
      [source:module_file(foo_ut, [{bar_test, ["nok = foo:bar()."]}])],
      fun(Dir,_)->
              [{unit, 1, 0}, {modules, 1, 1}] = xpdojo:test_files (Dir, Options),
              check_compile_message_reception(),
              {ok, Errors} =
                  receive
                      {unit, {error, foo_ut, Errors2}} ->
                          {ok, Errors2};
                      Message ->
                          Message
                  after 0 -> message_not_found
                  end,
              true = length (Errors) > 0
      end).

custom_report_function_unit_success_test() ->
    Options = 
        [{report_function, fun my_report_function/1},
	 {slave_name, test_slave}],
    use_and_purge_tree (
      [foo_ut(),foo()],
      fun(Dir,_) ->
              [{acceptance, 0, 0}, {unit, 1, 1}, {modules, 2,2}] = xpdojo:test_files (Dir, Options),
              check_compile_message_reception(),
              check_compile_message_reception(),
              ok = receive
                       {unit, {ok, foo_ut}} -> 
                           ok;
                       Message ->
                           Message
                   after 0 -> message_not_found
                   end                                     
      end).

custom_report_function_acceptance_error_test() ->
    Options = 
        [{report_function, fun my_report_function/1},
	{slave_name, test_slave}],
    use_and_purge_tree (
      [source:module_file(foo_acceptance, [{bar_test, ["nok = ok."]}])],
      fun(Dir,_)->
              [{acceptance, 1, 0}, {unit, 0, 0}, {modules, 1, 1}] = xpdojo:test_files (Dir, Options),
              check_compile_message_reception(),
              {ok, Errors} =
                  receive
                      {acceptance, {error, foo_acceptance, Errors2}} ->
                          {ok, Errors2};
                      Message ->
                          Message
                  after 0 -> message_not_found
                  end,
              true = length (Errors) > 0
      end).

custom_report_function_acceptance_success_test() ->
    Options = 
        [{report_function, fun my_report_function/1},
	 {slave_name, test_slave}],
    use_and_purge_tree (
      [one_acceptance_ok()],
      fun(Dir,_) ->
              [{acceptance, 1, 1}, {unit, 0, 0}, {modules, 1 ,1}] = xpdojo:test_files (Dir, Options),
              check_compile_message_reception(),
              ok = receive
                       {acceptance, {ok, one_acceptance}} -> 
                           ok;
                       Message ->
                           Message
                   after 0 -> message_not_found
                   end
      end).

check_compile_message_reception() ->
    ok = receive
	     {compile,_} ->
		 ok
	 after 0 -> 
		 compile_message_not_found 
	 end.

unchanged_for_same_path_test() ->
    use_and_purge_tree (
      [foo_acceptance(),
       {directory,"src",[foo(),bar()]},
       {directory,"unit",[foo_ut(), bar_ut()]}],
      fun (Dir,_) ->
              [{acceptance,1,0}, {unit,2,2}, {modules,5,5}] = xpdojo:test_files (Dir, options()),
              OtherDir = filename:join([Dir,"..",filename:basename(Dir)]),
              unchanged = xpdojo:test_files (OtherDir, options())
       end). 

my_report_function ({Phase, Term}) ->
    self() ! {Phase, Term}.

bad_links_test() ->
    adlib:use_tree(
      adlib:temporary_pathname(),
      [foo()],
      fun (Dir, _) ->
              [{acceptance,0,0}, {unit,0,0}, {modules,1,1}] = xpdojo:test_files (Dir, options()),
              Link = filename:join (Dir, "titi.erl"),
              Destination = filename:join (Dir, "nofile"),
              case file:make_symlink (Destination, Link) of
                  ok ->
                      unchanged = xpdojo:test_files (Dir, options());
                  {error, enotsup} ->
                      ok
              end
      end,
     fun (Dir, _) ->
            file:delete (filename:join (Dir, "titi.erl"))
     end).

no_beam_creation_test() ->
    adlib:use_tree(
      adlib:temporary_pathname(),
      [unique_erlang_module()],
      fun (Dir, _) ->
              [{acceptance,0,0}, {unit,0,0}, {modules,1,1}] = xpdojo:test_files (Dir, options()),
	      {ok,CurrentDir} = file:get_cwd(),
	      {error,enoent} = file:open(filename:join(CurrentDir,unique_erlang_module_beam()),[read])
      end).
    
