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

-module (testing).
-compile (export_all).

runner (Notify) ->
    process_flag (trap_exit, true),
    runner_loop (Notify, dict:new()).

runner_loop (Notify, Workers) ->
    receive
	{Token, test, Test} ->
	    Worker = spawn_link (fun() -> Test() end),
	    runner_loop (Notify, dict:store (Worker, Token, Workers));
	{'EXIT', Worker, normal} ->
	    Notify (dict:fetch (Worker, Workers), pass),
	    runner_loop (Notify, dict:erase (Worker, Workers));
	{'EXIT', Worker, Reason} ->
	    Notify (dict:fetch (Worker, Workers), {fail, Reason}),
	    runner_loop (Notify, dict:erase (Worker, Workers));
	stop ->
	    bye;
	Other ->
	    io:format("Here ~p ~n",[Other]),
	    throw ({unexpected_message, ?MODULE, Other, dict:to_list (Workers)})
    end.

run_modules (Slave, Modules, Pattern) ->
%%    run_modules(Modules, Pattern).
    lists:foldl(
       fun(Module, Acc) ->
	       {Node,_} = Slave,
	       FunctionsToTest = rpc:call(Node,testing,select_test_functions,[Module,Pattern]),
	      {FunctionCount,ModuleErrors} = run_functions(Slave,FunctionsToTest),
	      [{Module, FunctionCount, ModuleErrors} | Acc]
       end, 
      [], 
      Modules).

run_functions ({Node, Slave}, Functions) ->
    lists:foldl(
      fun(Fun, {Count,Errors}) ->
	      monitor_node (Node, true), 
	      Slave ! {{self(), Fun}, test, Fun},
	      receive
		  {nodedown, Node} ->
		      {Count + 1, [{slave_down, Fun} | Errors]};
		  {Fun, {fail, Reason}} ->
		      {Count + 1, [Reason | Errors]};
		  {Fun, pass} ->
		      {Count + 1, Errors}
%		  Other ->
%		      io:format("Unexpected Message ~p ~n",[Other]),
%		      {Count + 1, [{unexpected_message, "waiting for remote test run", Fun, Other} | Errors]}
	      after
		  100000 ->
		      {Count + 1, [{timeout, "waiting for remote test run", Fun} | Errors]}
	      end

%% 	      Master = self(),
%% 	      Run = fun() ->
%% 			    Result = (catch Fun()),
%% 			    Master ! {self(), Result}
%% 		    end,
	      
%% 	      Slave = spawn (, Run),
%% 	      monitor_node (Node, true), 
%% 	      receive
%% 		  {nodedown, Node} ->
%% 		      {Count + 1, [{slave_down, Fun} | Errors]};
%% 		  {Slave, {'EXIT', Reason}} ->
%% 		      {Count + 1, [Reason | Errors]};
%% 		  {Slave, _Other} ->
%% 		      {Count + 1, Errors};
%% 		  Other ->
%% 		      {Count + 1, [{unexpected_message, "waiting for remote test run", Fun, Other} | Errors]}
%% 	      after
%% 		  100000 ->
%% 		      {Count + 1, [{timeout, "waiting for remote test run", Fun} | Errors]}
%% 	      end
      end,
      {0,[]}, 
      Functions).

run_functions(Functions) when list(Functions) ->
    lists:foldl(
      fun(Fun, {Count,Errors}) ->
	      case catch Fun() of
		  {'EXIT', Reason} ->
		      {Count+1,[Reason|Errors]};
		  _Other ->
		      {Count+1,Errors}
	      end
      end, 
      {0,[]}, 
      Functions).

run_modules(Modules,Pattern) when list(Modules) ->
    lists:foldl(
       fun(Module, Acc) ->
	      {FunctionCount,ModuleErrors} = run_functions(select_test_functions(Module,Pattern)),
	      [{Module, FunctionCount, ModuleErrors} | Acc]
      end, 
	  [], 
	  Modules).

select_test_functions(Module,Pattern) when atom(Module), function(Pattern) ->
    [{Module,X} || {X,Y} <- Module:module_info(exports), Pattern(X), Y == 0].

use_and_purge_tree (Tree, Fun) ->
    adlib:use_tree (
      adlib:temporary_pathname(),
      Tree,
      Fun,
      fun (Dir, _) ->
	      compiling:purge_modules_from_directory (Dir),
	      purge_messages()
      end).

purge_messages() ->
    purge_messages(0,silent).

purge_messages(Tempo) ->
    purge_messages(Tempo,silent).

purge_messages(Tempo,Verbosity) ->
    receive
	Message ->
	    print_purged_message(Message,Verbosity),
	    purge_messages(Tempo)
    after Tempo ->
	    ok
    end.

print_purged_message(Message,verbose) ->
    io:fwrite("Purged: ~p~n", [Message]);
print_purged_message(_,_) ->
    ok.

wait_for_detectable_modification_time() ->
    timer:sleep (1000).

receive_one_from(Pid) ->
    receive
	{Pid, Message} ->
	    Message
    after 1000 ->
	    timeout
    end.
	
receive_one () ->
    receive
	Message ->
	    Message
    after 1000 ->
	    timeout
    end.
		
receive_all () ->
    receive_all ([]).

receive_all (Acc) ->
    receive
	Message ->
	    receive_all ([Message | Acc])
    after 1000 ->
	    Acc
    end.

file_system (Tree) ->
    spawn (?MODULE, file_system_loop, [tree_to_file_system_instructions (Tree, [])]).

file_system_loop (Instructions) ->
    receive
	{Client, Path, Commands} ->
	    Result = [{Command, dict:fetch ({Path, Command}, Instructions)} || Command <- Commands],
	    Client ! {self(), Path, Result},
	    file_system_loop (Instructions);
	stop ->
	    bye
    end.

tree_to_file_system_instructions ([{Path, directory, Time, Files} | Tail], Instructions) ->
    Remaining = [setelement (1, X, filename:join (Path, element (1, X))) || X <- Files] ++ Tail,
    New_instructions =
	[{{Path, type}, directory},
	 {{Path, directory_content}, [element (1, X) || X <- Files]},
	 {{Path, modification_time}, Time}]
	++ Instructions,
    tree_to_file_system_instructions (Remaining, New_instructions);
tree_to_file_system_instructions ([{Path, regular, Time} | Tail], Instructions) ->
    New_instructions =
	[{{Path, type}, regular},
	 {{Path, directory_content}, {error, enotdir}},
	 {{Path, modification_time}, Time}]
	++ Instructions,
    tree_to_file_system_instructions (Tail, New_instructions);
tree_to_file_system_instructions ([], Instructions) ->
    dict:from_list (Instructions).
