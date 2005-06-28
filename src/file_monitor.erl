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
-export([start/2, loop/3]).
-include_lib("kernel/include/file.hrl").

start (File_name, Pid) ->
    spawn(?MODULE, loop,[File_name, Pid, init]).

loop (File_name, Pid, Previous_state)->
    case file:read_file_info(File_name) of
	{error, enoent} ->
	    New_state = missing;
	{ok, File_info} ->
	    New_state = File_info#file_info.mtime
    end,
    handle_change(File_name, Pid, Previous_state, New_state).

handle_change (_, Pid, init, missing) ->
    Pid ! {self(), missing};
handle_change (_, Pid, _, missing) ->
    Pid ! {self(), deleted};
handle_change (File_name, Pid, Time, Time)->
    loop(File_name, Pid, Time);
handle_change (File_name, Pid, init, Time) ->
    Pid ! {self(), found},
    loop(File_name, Pid, Time);
handle_change (File_name, Pid, _, New_time) ->
    Pid ! {self(), modified},
    loop(File_name, Pid, New_time).
    
   
