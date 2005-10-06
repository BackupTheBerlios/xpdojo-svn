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
-export([start/2, stop/1, loop/3, start_master/2]).
-include_lib("kernel/include/file.hrl").

start (Name, Notify) ->
    spawn (?MODULE, start_master, [Name, Notify]).

start_master(Name, Notify) ->
    process_flag(trap_exit, true),
    Self = self(),
    start_link (
      Name,
      fun(Event, File) ->
	      Self ! {child_monitor, Event, File}
      end),
    master_loop(Name, Notify).

master_loop(Name, Notify) ->
    receive
	{child_monitor, Event, Name} when Event == nonexistent; Event == deleted ->
	    Notify (Event, Name),
	    bye;
	{child_monitor, Event, File} ->
	    Notify (Event, File),
	    master_loop(Name, Notify);
	{'EXIT', _, stopped_by_user} ->
	    exit (stopped_by_user)
    end.
					     
start_link (Name, Notify) ->
    spawn_link(?MODULE, loop, [init, functions(Name, Notify), []]).

stop (Pid) ->
    exit (Pid, stopped_by_user).

functions (Name, Notify) ->
    functions (filelib:is_dir(Name), Name, Notify).

functions (false, Name, Notify) ->
    {notify_fun (Name, Notify), file_signature_fun (Name)};
functions (true, Name, Notify) ->
    {spawn_and_notify_fun (Name, Notify), directory_signature_fun (Name)}.

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
			      NewPid = start_link (Name, Notification),
			      [{NewPid, Name} | Acc];
			  true ->
			      Acc
		      end
	      end,
	      [absolute_full_name],
	      Processes)
    end.

directory_signature_fun(Name) ->
    fun() ->
	    file:list_dir(Name)
    end.

file_signature_fun(Name) ->
    fun() ->
	    case file:read_file_info(Name) of
		{error, enoent} ->
		    {error, enoent};
		{ok, File_info} ->
		    File_info#file_info.mtime
	    end
    end.

loop (Signature, Functions = {Handle_change, Compute_signature}, State) ->
    Loop = fun (New_signature, New_state) ->
		   receive 
		       {'EXIT',_,stopped_by_user} ->
			   exit (stopped_by_user)
 		   after 0 -> 
			   loop (New_signature, Functions, New_state)
 		   end
	   end,
    transition (Loop, Handle_change, Signature, Compute_signature(), State).

transition (_, Handle_change, init, {error,enoent}, State) ->
    Handle_change (nonexistent, State),
    bye;
transition (_, Handle_change, _, {error,enoent}, State) ->
    Handle_change (deleted, State),
    bye;
transition (Loop, _, Signature, Signature, State)->
    Loop (Signature, State);
transition (Loop, Handle_change, init, Signature, State) ->
    New_state = Handle_change (found, State),
    Loop (Signature, New_state);
transition (Loop, Handle_change, _, New_signature, State) ->
    New_state = Handle_change (modified, State),
    Loop (New_signature, New_state).

