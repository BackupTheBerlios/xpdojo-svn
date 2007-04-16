%%% Copyright (c) 2004-2007 Dominic Williams, Nicolas Charpentier
%%% All rights reserved.
%%% See file COPYING.

-module (forge).
-export ([start/0]).
-import (orddict, [append/3, fetch/2, new/0, store/3]).

start () ->
    spawn_link (fun () -> init() end).

init() ->
    process_flag (trap_exit, true),
    loop ([], new(), dashboard:new()).

loop (Listeners, Smiths, Dashboard) ->
    receive
	stop ->
	    bye;
	{add_event_handler, F} ->
	    loop ([F | Listeners], Smiths, Dashboard);
	{_, found, File, _} ->
	    Smith = handle_new_file (filename:extension (File), File),
	    loop (Listeners, store (File, Smith, Smiths), Dashboard);
	{_, _, _, Status} = Event ->
	    New_dashboard = dashboard:update (Status, Dashboard),
	    notify (Event, New_dashboard, Listeners),
	    loop (Listeners, Smiths, New_dashboard)
    end.

notify ({_, _, Module, Status}, Dashboard, Listeners) ->
    lists:foreach (
      fun (F) ->
	      F ({event, {Module, Status}}),
	      F (Dashboard)
      end,
      Listeners).

handle_new_file (".erl", File) ->
    Forge = self(),
    spawn_link (fun () -> smith:start (File, Forge) end).

