%% Author: NicolasCharpentier
%% Created: 17 mars 2006
%% Description: TODO: Add desciption to green_bar_acceptance
-module(green_bar_acceptance).
-compile(export_all).
-import(testing,
	[use_and_purge_tree/2,
	 receive_one_from/1,
	 purge_messages/1]).

start_green_bar_test() ->
    use_and_purge_tree (
      [],
      fun (_Dir, _) ->
	      Pid = green_bar:start (),
	      true = is_process_alive (Pid),
						% Hack before next Test
	      exit (Pid, kill)
      end).

distant_start_green_bar_test() ->
    use_and_purge_tree (
      [],
      fun (_Dir, _) ->
	      Pid = green_bar:start (node()),
	      true = is_process_alive (Pid),
						% Hack before next Test
	      exit (Pid, kill)
      end).
    
stop_green_bar_test() ->
    use_and_purge_tree (
      [],
      fun (_Dir, _) ->
	      _Previous_processes = processes(),
	      Pid = green_bar:start (),
	      true = is_process_alive (Pid),
	      green_bar:stop(Pid),
   	      timer:sleep(3000),
						% same_elements = adlib:compare (Previous_processes, processes())
	      ok
      end).

kill_green_bar_test() ->
    use_and_purge_tree (
      [],
      fun (_Dir, _) ->
	      _Previous_processes = processes(),
	      Pid = green_bar:start (),
	      purge_messages(3000),
	      exit (Pid, kill),
	      false = is_process_alive (Pid),
	      timer:sleep(1000),
						%	      same_elements = adlib:compare (Previous_processes, processes())
	      ok
      end).

components_green_bar_test() ->
    use_and_purge_tree (
      [],
      fun (_Dir, _) ->
	      Pid = green_bar:start (),
	      Components = green_bar:components(Pid),
	      true = adlib:contains(acceptance,Components),
	      true = adlib:contains(unit,Components),
	      true = adlib:contains(modules,Components),
	      green_bar:stop(Pid)
      end).

red_color_by_defaut_test() ->
    Pid = green_bar:start (),
    adlib:use_tree (
      adlib:temporary_pathname(),
      [],
      fun (_Dir, _) ->
	      Components = green_bar:components(Pid),
	      GraphicalBar = [X || {A,X} <- Components, case A of acceptance ->true; unit-> true; modules-> true;_Other-> false end],
	      Assert = fun(Button) -> red = gs:read(Button,bg) end,
	      lists:foreach(Assert,GraphicalBar)
      end,
      fun(_Dir,_) ->
	      purge_messages(0),
    	  green_bar:stop(Pid)
      end).

reporting_message_test() ->
    Pid = green_bar:start (),
    adlib:use_tree (
      adlib:temporary_pathname(),
      [],
      fun (_Dir, _) ->
	      GraphicalBar = [X || {A,X} <- green_bar:components(Pid), case A of acceptance ->true; unit-> true; modules-> true;_Other-> false end],
      	GoodMessage = [{acceptance,3,3},{modules,1,1},{unit,0,0}],
	      green_bar:notify(Pid,GoodMessage),
	      timer:sleep(1000),
	      AssertGreen = fun(Button) -> green = gs:read(Button,bg) end,
	      lists:foreach(AssertGreen,GraphicalBar),
	      WrongMessage = [{acceptance,9,3},{modules,0,1},{unit,"aa",0}],
	      green_bar:notify(Pid,WrongMessage),
	      timer:sleep(1000),
	      AssertRed = fun(Button) -> red = gs:read(Button,bg) end,
	      lists:foreach(AssertRed,GraphicalBar)
      end,
      fun(_Dir,_) ->
	      purge_messages(0),
	      green_bar:stop(Pid)
      end).
   
