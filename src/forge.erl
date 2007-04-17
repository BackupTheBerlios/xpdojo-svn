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
	    Smith = file_handler (filename:extension (File), File),
	    loop (Listeners, add_smith (File, Smith, Smiths), Dashboard);
	{_, _, _, Status, Details} = Event ->
	    New_dashboard = dashboard:update (Status, Dashboard),
	    notify (Event, New_dashboard, Listeners),
	    loop (Listeners, Smiths, New_dashboard)
    end.

notify ({_, _, Module, Status, Details}, Dashboard, Listeners) ->
    lists:foreach (
      fun (F) ->
	      F ({event, {Module, Status, Details}}),
	      F (Dashboard)
      end,
      Listeners).

file_handler (".erl", File) ->
    Forge = self(),
    spawn_link (fun () -> smith:start (File, Forge) end);
file_handler (_, _) ->
    no_handler.

add_smith (File, Smith, Smiths) when is_pid (Smith) ->
    store (File, Smith, Smiths);
add_smith (_, no_handler, Smiths) ->
    Smiths.
