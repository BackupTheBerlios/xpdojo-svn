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

-module(filesystem).
-export([start/0, worker_loop/1, serve/1]).
-include_lib("kernel/include/file.hrl").

serve (Fun) ->
    Server = spawn (?MODULE, start, []),
    Result = Fun (Server),
    Server ! stop,
    Result.

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
	    Fun = worker_fun (Command),
	    Controller ! {self(), Path, [{Command, Fun (Path)}], Client},
	    worker_loop (Controller);
	stop ->
	    bye
    end.

worker_fun (type) ->
    fun (Path) ->
	    {ok, File_info} = file:read_file_info (Path),
	    File_info#file_info.type
    end;

worker_fun (directory_content) ->
    fun (Path) ->
	    {ok, Filename_list} = file:list_dir (Path),
	    Filename_list
    end.
