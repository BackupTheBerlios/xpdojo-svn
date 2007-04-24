%%% Copyright (c) 2004-2007 Dominic Williams, Nicolas Charpentier,
%%% Fabrice Nourisson, Jacques Couvreur, Virgile Delecolle.
%%% All rights reserved.
%%% See file COPYING.

-module(continuously_test_files_acceptance).
-compile(export_all).
-import(testing, [receive_one_from/1, use_and_purge_tree/2, use_and_purge_tree_with_file_system/2]).

test_with_tree_and_forge (Tree, Test) ->
    use_and_purge_tree_with_file_system (
      Tree,
      fun (Dir, File_system) ->
	      Listener = self (),
	      Server = forge:start (),
	      Server ! {add_event_handler, fun (Event) -> Listener ! {self (), Event} end},
	      Tell_server = fun (Event, File_name, FS) -> Server ! {self (), Event, File_name, FS} end,
	      Monitor = file_monitor:start (File_system, Dir, Tell_server),
	      try
		  Test (Server, Dir, File_system),
		  {purge, timeout} = {purge, receive_one_from (Server)},
		  ok
	      after
		  Server ! stop,
		  file_monitor:stop (Monitor)
	      end
      end).

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

unused_import_erlang_module() ->
    {file,"foo.erl",
     ["-module(foo).",
      "-import(orddict,[new/3]).",
      "-export([bar/0]).",
      "bar() -> ok."]}.
    
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
    test_with_tree_and_forge (
      [],
      fun (Forge, _, _) ->
	      timeout = receive_one_from (Forge)
      end).

tree_without_code_test() ->
    test_with_tree_and_forge (
      [{directory, "temporary",
	[{file, "toto.txt", []},
	 {file, "titi.xml", []}]},
       {directory, "temp2",
	[{file, "foo.beam", []},
	 {directory,"sub",
	  [{file, "truc", []},
	   {file, "machin.o", []}]}]}],
      fun (Forge, _, _) ->
	      timeout = receive_one_from (Forge)
      end).
      
single_module_test () ->
    test_with_tree_and_forge (
      [foo()],
      fun (Server, _, _) ->
	      {event, {"foo", {module, uncompiled}, _}} = receive_one_from (Server),
	      {dashboard, {{modules, 0, 0, 1}, {unit, 0, 0, 0}, {acceptance, 0, 0, 0}}} = receive_one_from (Server),
	      {event, {foo, {module, compiled}, Warnings}} = receive_one_from (Server),
	      {no_warnings, []} = {no_warnings, Warnings},
	      {dashboard, {{modules, 1, 0, 1}, {unit, 0, 0, 0}, {acceptance, 0, 0, 0}}} = receive_one_from (Server)
      end).

single_bad_module_test () ->
    test_with_tree_and_forge (
      [bad_bar()],
      fun (Server, _, _) ->
	      {event, {"bar", {module, uncompiled}, _}} = receive_one_from (Server),
	      {dashboard, {{modules, 0, 0, 1}, {unit, 0, 0, 0}, {acceptance, 0, 0, 0}}} = receive_one_from (Server),
	      {event, {"bar", {module, errors}, {Errors, _}}} = receive_one_from (Server),
	      {errors_exist, true} = {errors_exist, length (Errors) > 0},
	      {dashboard, {{modules, 0, 1, 1}, {unit, 0, 0, 0}, {acceptance, 0, 0, 0}}} = receive_one_from (Server)
      end).

multi_module_test() ->
    test_with_tree_and_forge (
      [foo(), bad_bar(), baz()],
      fun (Server, _, _) ->
	      Messages =
		  lists:reverse (
		    lists:foldl (
		      fun (_, Acc) ->
			      Received =
				  receive {Server, Msg} -> Msg
				  after 30000 -> timeout end,
			      [Received | Acc]
		      end,
		      [],
		      lists:seq (1, 12))),
	      check_multi_module_messages (Messages)
      end).

check_multi_module_messages (Messages) ->
    Make_pairs =
	fun (Event, {[], Pairs}) ->
		{[Event], Pairs};
	    (Dashboard, {[Event], Pairs}) ->
		{[], [{Event, Dashboard} | Pairs]}
	end,
    {[], Pairs} = lists:foldl (Make_pairs, {[], []}, Messages),
    check_event_dashboard_pairs (lists:reverse(Pairs)).

check_event_dashboard_pairs (Pairs) ->
    {Module_events, Compile_events} = lists:split (3, Pairs),
    check_module_event_pairs (Module_events),
    check_compile_event_pairs (Compile_events).

check_module_event_pairs (Pairs) ->
    {4, Modules} =
	lists:foldl (
	  fun ({{event, {Module, {module, uncompiled}, _}},
		{dashboard, {{modules, 0, 0, Count}, {unit, 0, 0, 0}, {acceptance, 0, 0, 0}}}},
	       {Count, Modules}) ->
		  {Count + 1, [Module | Modules]}
	  end,
	  {1, []},
	  Pairs),
    ["bar", "baz", "foo"] = lists:sort (Modules).

check_compile_event_pairs (Pairs) ->
    Simplified =
	[{Mod, Result, Yes, No, Total}
	 || {{event, {Mod, {module, Result}, _}}, {dashboard, {{modules, Yes, No, Total}, {unit, 0, 0, 0}, {acceptance, 0, 0, 0}}}}
		<- Pairs],
    Results =
	lists:keysort (1, [{Mod, Res} || {Mod, Res, _, _, _} <- Simplified]),
    [{baz, compiled}, {foo, compiled}, {"bar", errors}] = Results,
    lists:foldl (
      fun ({_, Result, Yes, No, 3}, {Old_yes, Old_no}) ->
	      case Result of
		  compiled ->
		      Yes = Old_yes + 1,
		      No = Old_no;
		  errors ->
		      Yes = Old_yes,
		      No = Old_no + 1
	      end,
	      {Yes, No}
      end,
      {0, 0},
      Simplified).

change_single_module_test() ->
    test_with_tree_and_forge (
      [foo()],
      fun (Forge, Dir, _) ->
	      {event, {"foo", {module, uncompiled}, _}} = receive_one_from (Forge),
              {dashboard, {{modules, 0, 0, 1}, _, _} }= receive_one_from (Forge),
	      {event, {foo, {module, compiled}, _}} = receive_one_from (Forge),
              {dashboard, {{modules, 1, 0, 1}, _, _}} = receive_one_from (Forge),
              file:write_file(filename:join(Dir,"foo.erl"),
                              "-module(foo).\n"
                              "-export([yo/0]).\n"
                              "yo() - yohoho."),
	      {event, {foo, {module, uncompiled}, _}} = receive_one_from (Forge),
              {dashboard, {{modules, 0, 0, 1}, _, _}} = receive_one_from (Forge),
	      {event, {foo, {module, errors}, _}} = receive_one_from (Forge),
              {dashboard, {{modules, 0, 1, 1}, _, _}} = receive_one_from (Forge),
              file:write_file(filename:join(Dir,"foo.erl"),
                              "-module(foo).\n"
                              "-export([yo/0]).\n"
                              "yo() -> yohoho."),
	      {event, {foo, {module, uncompiled}, _}} = receive_one_from (Forge),
              {dashboard, {{modules, 0, 0, 1}, _, _}} = receive_one_from (Forge),
	      {event, {foo, {module, compiled}, _}} = receive_one_from (Forge),
              {dashboard, {{modules, 1, 0, 1}, _, _}} = receive_one_from (Forge)
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
			{Index, Expected_message} = {Index, receive Message -> Message after 5000 -> timeout end},
			Index + 1
		end,
		1,
		[{fun() -> Silly ! {Self, key} end,
		  charpi},
		 {fun() -> testing_server:start (Dir, Notification, options()) end,
		  {Key, [{acceptance, 0, 0}, {unit, 1, 1}, {modules, 3, 3}]}},
		 {fun() -> timer:sleep(1000),Silly_server_writer("domi") end,
%% No modification state so testing_server don't do anything !!!		  {Key, [{acceptance, 0, 0}, {unit, 1, 1}, {modules, 3, 3}]}}, 
		  timeout},
		 {fun() -> Silly ! {Self, key} end,
		  charpi},
		 {fun() -> file:write_file (filename:join (Dir, "bar.erl"), source:module (bar, [foo])) end,
		  {Key, [{acceptance, 0, 0}, {unit, 1, 1}, {modules, 4, 4}]}},
		 {fun() -> testing_server:stop() end,
		  timeout},
		 {fun() -> file:write_file (filename:join (Dir, "pepe.erl"), source:module (pepe, [juan])) end,
		  timeout}])
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

%% unchanged_for_same_path_test() ->
%%     use_and_purge_tree (
%%       [foo_acceptance(),
%%        {directory,"src",[foo(),bar()]},
%%        {directory,"unit",[foo_ut(), bar_ut()]}],
%%       fun (Dir,_) ->
%%               [{acceptance,1,0}, {unit,2,2}, {modules,5,5}] = xpdojo:test_files (Dir, options()),
%%               OtherDir = filename:join([Dir,"..",filename:basename(Dir)]),
%%               unchanged = xpdojo:test_files (OtherDir, options())
%%        end). 

my_report_function ({Phase, Term}) ->
    self() ! {Phase, Term}.

%% bad_links_test() ->
%%     adlib:use_tree(
%%       adlib:temporary_pathname(),
%%       [foo()],
%%       fun (Dir, _) ->
%%               [{acceptance,0,0}, {unit,0,0}, {modules,1,1}] = xpdojo:test_files (Dir, options()),
%%               Link = filename:join (Dir, "titi.erl"),
%%               Destination = filename:join (Dir, "nofile"),
%%               case file:make_symlink (Destination, Link) of
%%                   ok ->
%%                       unchanged = xpdojo:test_files (Dir, options());
%%                   {error, enotsup} ->
%%                       ok
%%               end
%%       end,
%%      fun (Dir, _) ->
%%             file:delete (filename:join (Dir, "titi.erl"))
%%      end).

no_beam_creation_test() ->
    adlib:use_tree(
      adlib:temporary_pathname(),
      [unique_erlang_module()],
      fun (Dir, _) ->
              [{acceptance,0,0}, {unit,0,0}, {modules,1,1}] = xpdojo:test_files (Dir, options()),
	      {ok,CurrentDir} = file:get_cwd(),
	      {error,enoent} = file:open(filename:join(CurrentDir,unique_erlang_module_beam()),[read])
      end).
    
unused_import_test() ->
    test_with_tree_and_forge (
      [unused_import_erlang_module()],
      fun (Server, _, _) ->
	      {event, {"foo", {module, uncompiled}, _}} = receive_one_from (Server),
	      {dashboard, {{modules, 0, 0, 1}, {unit, 0, 0, 0}, {acceptance, 0, 0, 0}}} = receive_one_from (Server),
	      {event, {foo, {module, compiled}, Warnings}} = receive_one_from (Server),
	      [{_,[{_,_,{unused_import,{{new,3},orddict}}}]}] = Warnings,
	      {dashboard, {{modules, 1, 0, 1}, {unit, 0, 0, 0}, {acceptance, 0, 0, 0}}} = receive_one_from (Server)
      end).
