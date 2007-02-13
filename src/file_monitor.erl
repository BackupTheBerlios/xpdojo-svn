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
-export([start/3, stop/1, loop/4, bind_content/1]).
-include_lib("kernel/include/file.hrl").

start (Filesystem, Directory, Notify) ->
    spawn (?MODULE, loop, [Filesystem, filename:absname(Directory), Notify, []]).

loop (Filesystem, Directory, Notify, Tree) ->
    case filesystem:list_recursively (Filesystem, Directory, [type, modification_time])  of
	{error, enoent} ->
	    Notify (nonexistent, Directory, Filesystem);
	List when is_list(List) ->
	    NewTree = 
		[{Item, Time} || {Item, Type, Time} <- List, Type == regular],
	    report (directory_tree:changes (Tree, NewTree), Notify, Filesystem),
	    receive
		stop ->
		    bye
	    after
		500 ->
		    loop (Filesystem, Directory, Notify, NewTree)
	    end
    end.

report (Changes, Notify, Filesystem) when is_list (Changes) ->
    lists:foreach (
      fun (Event) ->
	      report_event (Event, Notify, Filesystem)
      end,
      Changes).

report_event ({Event, Files}, Notify, Filesystem) ->
    lists:foreach (
      fun (File) ->
	      Notify(Event, File, Filesystem)
      end,
      Files).

stop (Pid) ->
    Pid ! stop.
		   
bind_content(Fun) ->
    fun(Event, Filename, Filesystem) when Event == found; Event == modified ->
	    Filesystem ! {self(), Filename, [content]},
	    receive {Filesystem, Filename, [{content, Content}]} ->
		    Fun ({Event, Content}, Filename, Filesystem)
	    end;
       (Event, Filename, Filesystem) ->
	    Fun(Event, Filename, Filesystem)
    end.
