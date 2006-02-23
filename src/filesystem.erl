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

-module (filesystem).
-export ([start/0, worker_loop/1, serve/1]).
-export ([directory_content/1, type/1, modification_time/1]).
-include_lib ("kernel/include/file.hrl").

serve (Fun) ->
    Caller = self(),
    Spawned =
	spawn (
	  fun() ->
		  Server = spawn (?MODULE, start, []),
		  case (catch Fun (Server)) of
		      {'EXIT', Reason} ->
			  Return = fun () -> exit(Reason) end;
		      Other ->
			  Return = fun () -> Other end
		  end,
		  Server ! stop,
		  Caller ! {self(), Return}
	  end),
    receive
	{Spawned, Return} ->
	    Return()
    end.
	      
start() ->
    process_flag (trap_exit, true),
    serve_client (new_worker (self())).

new_worker (Server) ->
    spawn_link (?MODULE, worker_loop, [Server]).

serve_client (Worker) ->
    receive
	{Client, Path, [Command]} ->
	    Worker ! {Command, Path, Client},
	    serve_worker (Worker, Path, Client);
	stop ->
	    Worker ! stop
    end.

serve_worker (Worker, Path, Client) ->
    receive
	{'EXIT', Worker, Reason} ->
	    Client ! {self(), Path, {error, Reason}},
	    serve_client (new_worker (self ()));
	{Worker, Path, Result, Client} ->
	    Client ! {self(), Path, Result},
	    serve_client (Worker)
    end.
    
worker_loop (Controller) ->
    receive
	{Command, Path, Client} ->
%%	    Fun = worker_fun (Command),
	    Controller ! {self(), Path, [{Command, ?MODULE:Command (Path)}], Client},
	    worker_loop (Controller);
	stop ->
	    bye
    end.

type (Path) ->
    {ok, File_info} = file:read_file_info (Path),
    File_info#file_info.type.

directory_content (Path) ->
    {ok, Filename_list} = file:list_dir (Path),
    Filename_list.

modification_time (Path) ->
    {ok, File_info} = file:read_file_info (Path),
    File_info#file_info.mtime.
