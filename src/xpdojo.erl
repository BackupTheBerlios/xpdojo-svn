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

-module(xpdojo).

-export([test_files/1, test_files/2, default_options/0, error_only_report/0]).

default_options () ->
    [{unit_modules_filter, adlib:ends_with("ut")},
     {unit_functions_filter, adlib:ends_with("test")},
     {report_function, fun simple_report/1},
     {slave_name, slave}].

test_files (Directory) ->
    test_files (Directory, default_options ()).

start_slave(Name) ->
    {ok, Host_name} = inet:gethostname(),
    Node =
	case slave:start (list_to_atom (Host_name), Name) of
	    {ok, Slave} -> Slave;
	    {error, {already_running, Slave}} -> Slave;
	    {error, Reason} -> exit({noslave, Reason})
	end,
    monitor_node(Node, true),
    Notify = fun({Pid,Test_id}, Message) -> Pid ! {Test_id, Message} end,
    Process = spawn (testing, runner, [Node, Notify]),
    {Node, Process}.

stop_slave(Options) ->
    {value, {slave, {Node, _}}} = lists:keysearch(slave, 1, Options),
    monitor_node(Node, false),
    ok = slave:stop(Node).

update_options(Options) ->
    Updated = adlib:update_options (Options, default_options ()),
    {value, {slave_name, Name}} = lists:keysearch (slave_name, 1, Updated),
    [{slave, start_slave (Name)} | Updated].

test_files (Directory, Options) ->
    Dir = adlib:normalise_path (filename:absname (Directory)),
    All_options = update_options (Options),
    Unit = unit (All_options),
    Compile = compile (All_options),
    Post = post_compile(All_options),
    DifferenceOnSlave = find_differences_on_slave(All_options),
    Result = 
	with (Dir,
	      [fun find_modules/2,
	       DifferenceOnSlave,
	       Compile,
	       Post,
	       fun (_Dir2, Modules) -> acceptance (All_options,Unit(Modules)) end],
	      []),
    stop_slave(All_options),
    Result.

with (Directory, [Fun|T], Acc) ->
    case Fun (Directory, Acc) of
        {stop, Result} ->
            Result;
        New_acc ->
            with (Directory, T, New_acc)
    end;
with (_Directory, [], Acc) ->
    Acc.

post_compile (Options) ->
    {value, {slave, {Slave, _}}} = lists:keysearch (slave, 1, Options),
    fun (_Directory, {{compiled, Compiled}, {failed, Failed}}) ->
	    lists:foreach (
	      fun({Module,Binary}) ->
		      rpc:call (Slave, code, purge, [Module]),
		      {module, Module} = rpc:call (Slave, code, load_binary, [Module,"",Binary])
	      end,
	      Compiled),
	    lists:foreach (
	      fun(Module) ->
		      rpc:call (Slave, code, purge, [Module]),
		      rpc:call (Slave, code, delete, [Module])
	      end,
	      Failed),
	    {length (Compiled) + length (Failed), Compiled}
    end.

find_differences_on_slave(Options) ->
    {value, {slave, {Slave, _}}} = lists:keysearch (slave, 1, Options),
    fun (Directory, Files) ->
	    Loaded_modules = rpc:call(Slave,compiling,loaded_modules,[Directory]),
	    case rpc:call(Slave,compiling,differences,[Loaded_modules, Files]) of
		[] ->
		    {stop, unchanged};
		_Changes ->
		    Files
	    end
    end.

find_modules (Directory, _) ->
    case source:erlang_files (Directory) of
        [] ->
            {stop, no_source_files};
        Files ->
            Files
    end.

compile (Options) when is_list (Options) ->
    {value, {_,  Report_function}} = lists:keysearch (report_function, 1, Options),
    compile (Report_function);
compile (Report_function) when is_function (Report_function) ->
    fun (Dir, Files) -> compiling:compile (Dir, Files, Report_function) end.
    
unit (Options) when is_list(Options)->
    {value, {_,  Module_filter}} = lists:keysearch (unit_modules_filter, 1, Options),
    {value, {_,  Function_filter}} = lists:keysearch (unit_functions_filter, 1, Options),
    {value, {_,  Report_function}} = lists:keysearch (report_function, 1, Options),
    {value, {_, Slave}} = lists:keysearch (slave, 1, Options),
    unit ({Slave, Module_filter, Function_filter, Report_function});

unit (Options) when is_tuple (Options) ->
    fun ({Module_count, Compiled_modules}) ->
            Work = 'work?' (Module_count, Compiled_modules),
            unit_work (Options, Work, Module_count, Compiled_modules)
    end.

'work?' (0,_) ->
    no_work;
'work?' (Module_count, Compiled_modules) when length (Compiled_modules) < Module_count ->
    no_work;
'work?' (_,_) ->
    has_work.

unit_work (Options, has_work, Module_count, Compiled_modules) ->
    test_pass (Options, Compiled_modules, unit, [{modules, Module_count, Compiled_modules}]);
unit_work (_, no_work, Module_count, Compiled_modules) ->
    [{modules, Module_count, Compiled_modules}].

acceptance (_,[{modules, Count, Compiled_modules}]) ->
    [{modules, Count, length (Compiled_modules)}];
acceptance (_,[{unit, Total, Successes}, {modules, Count, Compiled_modules}]) when Successes < Total ->
    [{unit, Total, Successes}, {modules, Count, length (Compiled_modules)}];
acceptance (Options,[Unit_summary, {modules, Module_total, Compiled_modules}]) ->
    {value, {_,  Report_function}} = lists:keysearch (report_function, 1, Options),
    {value, {_, Slave}} = lists:keysearch (slave, 1, Options),
    test_pass (
      {Slave, adlib:ends_with ("_acceptance"), adlib:ends_with ("_test"), Report_function}, 
      Compiled_modules, 
      acceptance, 
      [Unit_summary, {modules,Module_total,length(Compiled_modules)}]).

test_pass ({Slave, Module_filter, Function_filter, Report_function}, Modules, Pass_name, Acc) ->
    Tests = [X || {X,_} <- Modules, Module_filter(X)],
    Results = testing:run_modules (Slave, Tests, Function_filter),
    {Total, FailureCount, Failures}
        = lists:foldl(
            fun({Module, Count, []}, {Sum, FailureCount, FailuresAcc}) ->
                    {Count + Sum, FailureCount, [Module|FailuresAcc]};
               ({Module, Count, Failures}, {Sum, FailureCount, FailuresAcc}) ->
                    {Count + Sum, FailureCount+length(Failures), [{Module,Failures} | FailuresAcc]}
            end,
            {0, 0, []},
            Results),

    report(Failures, Pass_name, Report_function),
    [{Pass_name, Total, Total - FailureCount} | Acc ].
    
report ([{Module, Failures} | T], Pass_name, Report_function) ->
    Report_function ({Pass_name, {error, Module, Failures}}),
    report (T, Pass_name, Report_function);
report ([Module | T], Pass_name, Report_function) ->
    Report_function ({Pass_name, {ok, Module}}),
    report (T, Pass_name, Report_function);
report ([], _, _) ->
    ok.

simple_report ({Phase, Term}) ->
    io:fwrite("~p: ~p~n", [Phase, Term]).

error_only_report() ->
    fun error_only_report/1.

error_only_report({Phase,{error,Error,Reason,Extra}}) ->
    io:fwrite("~p: ~p~n", [Phase, {error,Error,Reason,Extra}]);
error_only_report({Phase,{error,Error,Reason}}) ->
    io:fwrite("~p: ~p~n", [Phase, {error,Error,Reason}]);
error_only_report(_) ->
    nothing.
