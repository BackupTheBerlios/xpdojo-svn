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

-module(file_monitor).
-export([start/2, loop/4]).
-include_lib("kernel/include/file.hrl").

start (File_name, Notification) ->
    start (filelib:is_dir(File_name), File_name, Notification).

start (false, File_name, Notification) ->
    spawn(?MODULE, loop,[File_name, notify_fun (File_name, Notification), init, []]);
start (true, Directory, Notification) ->
    spawn (?MODULE, loop, [Directory, spawn_and_notify_fun (Directory, Notification), init, []]).

notify_fun(File_name, Notification) ->
    fun (Event, State) ->
	    Notification (Event, File_name),
	    State
    end.

spawn_and_notify_fun (File_name, Notification) ->
    fun (Event, _) when Event == nonexistent; Event == deleted->
	    Notification (Event, File_name);
	(Event, Processes) when Event == found; Event == modified ->
	    Notification (Event, File_name),
	    adlib:fold_files_without_recursion (
	      File_name,
	      fun ([Name], Acc) ->
		      case lists:keymember (Name, 2, Acc) of
			  false ->
			      NewPid = start (Name, Notification),
			      [{NewPid, Name} | Acc];
			  true ->
			      Acc
		      end
	      end,
	      [absolute_full_name],
	      Processes)
    end.

loop (File_name, Action, Previous_event, State)->
    case file:read_file_info(File_name) of
	{error, enoent} ->
	    Event = nonexistent;
	{ok, File_info} ->
	    Event = File_info#file_info.mtime
    end,
    Loop = fun (Event2, State2) ->
		   loop (File_name, Action, Event2, State2)
	   end,
    handle_change(Loop, Action, Previous_event, Event, State).

handle_change (_, Action, init, nonexistent, State) ->
    Action (nonexistent, State),
    bye;
handle_change (_, Action, _, nonexistent, State) ->
    Action (deleted, State),
    bye;
handle_change (Loop, _, Time, Time, State)->
    Loop (Time, State);
handle_change (Loop, Action, init, Time, State) ->
    New_state = Action (found, State),
    Loop (Time, New_state);
handle_change (Loop, Action, _, New_time, State) ->
    New_state = Action (modified, State),
    Loop (New_time, New_state).

