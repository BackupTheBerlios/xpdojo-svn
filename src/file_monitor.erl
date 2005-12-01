%%% Copyright (c) 2005 Dominic Williams
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
-export([start/2, stop/1, loop/3]).
-include_lib("kernel/include/file.hrl").

start (Name, Notify) ->
    spawn (?MODULE, loop, [Name, Notify, []]).

loop (Name, Notify, Tree) ->
    case file:read_file_info(Name) of
	{error, enoent} ->
	    Notify (nonexistent, Name);
	{ok, _} ->
	    NewTree =
		adlib:fold_files (
		  Name,
		  fun ([regular, File_name, Modification_time], Acc) ->
			  [{File_name, Modification_time} | Acc];
		      (_, Acc) ->
			  Acc
		  end,
		  [type, absolute_full_name, modification_time],
		  []),
	    report (directory_tree:changes (Tree, NewTree), Notify),
	    receive
		stop ->
		    bye
	    after
		0 ->
		    loop (Name, Notify, NewTree)
	    end
    end.

report (Changes, Notify) when is_list (Changes) ->
    lists:foreach (
      fun (Event) ->
	      report_event (Event, Notify)
      end,
      Changes).

report_event ({Event, Files}, Notify) ->
    lists:foreach (
      fun (File) ->
	      Notify(Event, File)
      end,
      Files).

stop (Pid) ->
    Pid ! stop.
		   
