%%% Copyright (c) 2004-2007 Dominic Williams, Nicolas Charpentier
%%% All rights reserved.
%%% See file COPYING.

-module (testing).
-compile (export_all).

runner (Node, Notify) ->
    process_flag (trap_exit, true),
    runner_loop (Node, Notify, dict:new()).

runner_loop (Node, Notify, Workers) ->
    receive
	{Token, test, Test} ->
	    Worker = spawn(Node, fun() -> receive start -> Test() end end),
	    erlang:monitor (process, Worker),
	    Worker ! start,
	    runner_loop (Node, Notify, dict:store (Worker, Token, Workers));
	{'DOWN', _, process, Worker, normal} ->
	    handle (Notify, pass, Worker, Workers, Node);
	{'DOWN', _, process, Worker, Reason} ->
	    handle (Notify, {fail, Reason}, Worker, Workers, Node);
	stop ->
	    io:fwrite("~p ~p received stop~n", [node(), self()]),
	    bye;
	Other ->
	    io:fwrite("~p ~p received Other: ~p~n", [node(), self(), Other]),
	    throw ({unexpected_message, ?MODULE, Other, dict:to_list (Workers)})
    end.

handle (Notify, Message, Worker, Workers, Node) ->
    Notify (dict:fetch (Worker, Workers), Message),
    runner_loop (Node, Notify, dict:erase (Worker, Workers)).

run_modules (Slave, Modules, Pattern) ->
    lists:foldl(
       fun(Module, Acc) ->
	       FunctionsToTest = select_test_functions(Slave, Module, Pattern),
	       {FunctionCount,ModuleErrors} = run_functions(Slave,FunctionsToTest),
	       [{Module, FunctionCount, ModuleErrors} | Acc]
       end, 
      [], 
      Modules).

run_functions ({Node, Slave}, Functions) ->
    lists:foldl(
      fun(Fun, {Count,Errors}) ->
	      Slave ! {{self(), Fun}, test, Fun},
	      receive
		  {nodedown, Node} ->
		      {Count + 1, [{slave_down, Fun} | Errors]};
		  {Fun, {fail, Reason}} ->
		      {Count + 1, [Reason | Errors]};
		  {Fun, pass} ->
		      {Count + 1, Errors}
	      after
		  100000 ->
		      {Count + 1, [{timeout, "waiting for remote test run", Fun} | Errors]}
	      end
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

select_test_functions ({Node,_}, Module, Pattern) when atom (Module), function (Pattern) ->
    [{Module, X} || {X, Y} <- rpc:call (Node, Module, module_info, [exports]), Pattern(X), Y == 0].

use_and_purge_tree (Tree, Fun) ->
    adlib:use_tree (
      adlib:temporary_pathname(),
      Tree,
      Fun,
      fun (Dir, _) ->
	      compiling:purge_modules_from_directory (Dir),
	      purge_messages()
      end).

use_and_purge_tree_with_file_system(Tree, Fun) ->
    use_and_purge_tree(
      Tree,
      fun(Dir, _) ->
	      filesystem:serve(
		fun(F) ->
			Fun(Dir, F)
		end)
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
