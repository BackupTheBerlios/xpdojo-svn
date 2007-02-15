%%% Copyright (c) 2005 Dominic Williams, Nicolas Charpentier
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
-export ([content/1,md5/1]).
-export ([list_recursively/2, list_recursively/3]).
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
	{Client, Path, Commands} ->
	    Worker ! {Commands, Path, Client},
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
	{Commands, Path, Client} ->
	    Result = [{Command, ?MODULE:Command (Path)} || Command <- Commands],
	    Controller ! {self(), Path, Result, Client},
	    worker_loop (Controller);
	stop ->
	    bye
    end.

type (Path) ->
    case file:read_file_info (Path) of
	{ok, File_info} ->
	    File_info#file_info.type;
	Other ->
	    Other
    end.

directory_content (Path) ->
    case file:list_dir (Path) of
	{ok, Filename_list} ->
	    Filename_list;
	{error,eio} ->
	    {error,enotdir};
	Other ->
	    Other
    end.

modification_time (Path) ->
    case file:read_file_info (Path) of
	{ok, File_info} ->
	    File_info#file_info.mtime;
	Other ->
	    Other
    end.

md5 (Path) ->
    case type(Path) of
	regular ->
	    erlang:md5(content(Path));
	_ ->
	    []
    end.
    

content (Path) ->
    {ok, Binary} = file:read_file (Path),
    binary_to_list (Binary).

list_recursively (F, R) ->
    list_recursively (F, R, []).

list_recursively (File_system, Root, Options) ->
    Caller = self(),
    Worker =
	spawn_link (
	  fun() ->
		  File_system ! {self(), Root, [directory_content]},
		  list_recursively_loop ([], 1, Options, Caller)
	  end),
    receive {Worker, List} ->
	    List
    end.

list_recursively_loop (Acc, 0, _, Caller) ->
    Caller ! {self(), Acc};
list_recursively_loop (Acc, Pending, Options, Caller) ->
    receive
	{_, _, [{directory_content, {error, Reason}}]} ->
	    Caller ! {self(), {error, Reason}};
	{File_system, Path, [{directory_content, Content}]} ->
	    Message_count = lists:foldl (
	      fun (Entry, Count) -> 
		      File_system ! {self(), filename:join (Path, Entry), [type | Options]},
		      Count + 1
	      end,
	      0,
	      Content),
	    list_recursively_loop (Acc, Pending -1 + Message_count, Options, Caller);
	{_, Path, [{type, regular} | Option_results]} ->
	    list_recursively_loop ([pack (Path, Option_results) | Acc], Pending - 1, Options, Caller);
	{File_system, Path, [{type, directory} | Option_results]} ->
	    File_system ! {self(), Path, [directory_content]},
	    list_recursively_loop ([pack (Path, Option_results) | Acc], Pending, Options, Caller);
	{_, _, {error, _}} ->
	    list_recursively_loop (Acc, Pending - 1, Options, Caller);
	{_, _, [{type, {error, _}} | _]} ->
	    list_recursively_loop (Acc, Pending - 1, Options, Caller);
	Other ->
	    Caller ! {self(), {unexpected_message, Other, Acc}}
    after 2000 ->
	    Caller ! {self(), {timeout, Acc}}
    end.

pack (Path, []) ->
    Path;
pack (Path, Options) ->
    list_to_tuple ([Path | [element (2, X) || X <- Options]]).
