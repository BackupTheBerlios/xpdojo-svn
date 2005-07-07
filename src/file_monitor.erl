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
-export([start/2, loop/3, directory_loop/3]).
-include_lib("kernel/include/file.hrl").

start ({file, File_name}, Pid) ->
    spawn(?MODULE, loop,[File_name, Pid, init]);
start (Directory, Pid) ->
    spawn (?MODULE, directory_loop, [Directory, Pid, init]).

directory_loop (Directory, Pid, Previous_state) ->
    % On peut commencer crade:
    case file:read_file_info(Directory) of
	{error, enoent} ->
	    New_state = missing;
	{ok, File_info} ->
	    Action = fun([regular,Name],Acc)->
			     [Name|Acc];
			(_,Acc) ->
			     Acc
		     end,
	    New_state = adlib:fold_files(Directory,Action,[type,relative_full_name],[])
    end,
    Notify = fun (State) ->
		     Pid ! {self(), State}
	     end,
    Loop = fun (State) ->
		   directory_loop(Directory, Pid, State)
	   end,
    handle_directory_change(Loop, Notify, Previous_state, New_state).

handle_directory_change(_, Notify, _, missing) ->
    Notify(missing);
handle_directory_change(Loop, Notify, init, [])->
    Notify(empty),
    Loop([]);
handle_directory_change(Loop, Notify, [], [File_name]) ->
    Notify({new_file, File_name}),
    Loop([File_name]);
handle_directory_change(Loop, Notify, Unchanged, Unchanged) ->
    Loop(Unchanged);
handle_directory_change(_, _, [X], []) ->
    bye.

loop (File_name, Pid, Previous_state)->
    case file:read_file_info(File_name) of
	{error, enoent} ->
	    New_state = missing;
	{ok, File_info} ->
	    New_state = File_info#file_info.mtime
    end,
    Notify = fun (State) ->
		     Pid ! {self(), State}
	     end,
    Loop = fun (State) ->
		   loop (File_name, Pid, State)
	   end,
    handle_change(Loop, Notify, Previous_state, New_state).

handle_change (_, Notify, init, missing) ->
    Notify (missing);
handle_change (_, Notify, _, missing) ->
    Notify (deleted);
handle_change (Loop, _, Time, Time)->
    Loop (Time);
handle_change (Loop, Notify, init, Time) ->
    Notify (found),
    Loop (Time);
handle_change (Loop, Notify, _, New_time) ->
    Notify (modified),
    Loop (New_time).
