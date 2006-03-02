%%% copyright (c) 2005 Dominic Williams
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

-module(filesystem_ut).
-compile(export_all).
-import(testing, [use_and_purge_tree/2, receive_one/0]).

commands_test() ->
    lists:foreach (
      fun (Test) ->
	      use_and_purge_tree (
		[{file, "toto", []},
		 {directory, "tmp", [{file, "intmp", []}]},
		 {file, "other.xml", []}],
		fun (Dir, _) ->
			Previous_processes = processes(),
			Pid = filesystem:serve (
				fun (F) ->
					Test (F, Dir),
					true = is_process_alive (F),
					F
				end),
			timer:sleep(1000),
			false = is_process_alive (Pid),
			same_elements = adlib:compare (Previous_processes, processes())
		end)
      end,
      [fun directory_type/2,
       fun regular_type/2,
       fun enoent/2,
       fun directory_content/2,
       fun enotdir/2,
       modification_time (erlang:localtime ())]).

directory_type (Filesystem, Dir) ->
    Filesystem ! {self(), Dir, [type]},
    {Filesystem, Dir, [{type, directory}]} = receive_one ().
	      
regular_type (Filesystem, Dir) ->			   
    Filename = filename:join (Dir, "toto"),
    Filesystem ! {self(), Filename, [type]},
    {Filesystem, Filename, [{type, regular}]} = receive_one ().

enoent (Filesystem, Dir) ->
    Filename = filename:join (Dir, "titi_is_not_toto"),
    Filesystem ! {self(), Filename, [type]},
    {Filesystem, Filename, {error, Reason}} = receive_one (),
    true = adlib:contains ({error, enoent}, Reason).
    
directory_content (Filesystem, Dir) ->
    Filesystem ! {self(), Dir, [directory_content]},
    {Filesystem, Dir,
     [{directory_content, List}]} = receive_one (),
    same_elements = adlib:compare (["other.xml", "tmp", "toto"], List).

enotdir (Filesystem, Dir) ->
    Filename = filename:join (Dir, "toto"),
    Filesystem ! {self(), Filename, [directory_content]},
    {Filesystem, Filename, {error, Reason}} = receive_one (),
    true = adlib:contains ({error, enotdir}, Reason).

modification_time (Before) ->
    fun (Filesystem, Dir) ->
	    Filesystem ! {self(), Dir, [modification_time]},
	    {Filesystem, Dir, [{modification_time, Time}]} = receive_one(),
	    {1, Before, Time, true} = {1, Before, Time, Before =< Time},
	    After = erlang:localtime (),
	    {2, Time, After, true} = {2, Time, After, Time =< After}
    end.
    
serve_a_crashing_fun_test () ->
    Previous_processes = processes(),
    catch filesystem:serve (fun (_) -> exit (suicide) end),
    timer:sleep (1000),
    same_elements = adlib:compare (Previous_processes, processes()).
    
serve_a_crashing_client_test () ->
    Previous_processes = processes(),
    Test = self(),
    Spawned =
	spawn (
	  fun () ->
		  filesystem:serve (
		    fun (F) ->
			    F ! {self(), "Blah", [type]},
			    Test ! {self(), file_system_server_ready},
			    receive
				{Test, finish_now_please} ->
				    finished
			    end
		    end)
	  end),
    receive
	{File_system_server, file_system_server_ready} ->
	    exit (Spawned, kill),
	    false = is_process_alive (Spawned),
	    File_system_server ! {self(), finish_now_please},
	    timer:sleep(2000),
	    same_elements = adlib:compare (Previous_processes, processes())
    after 3000 ->
	    throw (timeout)
    end.

%%% multiple_requests_test () ->
%%%     ok = not_coded.
fake_file_system (Instructions) ->
    receive
	{Client, Path, [Command]} ->
	    Client ! {self(), Path, [{Command, dict:fetch ({Path, Command}, Instructions)}]},
	    fake_file_system (Instructions);
	stop ->
	    bye
    end.

list_recursively_test () ->
    lists:foreach (
      fun ({Root, Fake_instructions, Expected_result}) ->
	      F = spawn (?MODULE, fake_file_system, [dict:from_list (Fake_instructions)]),
	      Result = filesystem:list_recursively (F, Root),
	      F ! stop,
	      same_elements = adlib:compare (Expected_result, Result)
      end,
      [{"/tmp",  [{{"/tmp", directory_content}, []}], []},
       {"/home",
	[{{"/home", directory_content}, ["a", "b"]},
	 {{"/home/a", type}, regular},
	 {{"/home/b", type}, regular}],
	["/home/a", "/home/b"]} ,
       {"/root/dir",
	[{{"/root/dir", directory_content}, ["file", "sub", "file2", "sub2"]},
	 {{"/root/dir/file", type}, regular},
	 {{"/root/dir/file2", type}, regular},
 	 {{"/root/dir/sub/file3", type}, regular},
 	 {{"/root/dir/sub2/file4", type}, regular},
 	 {{"/root/dir/sub2/sub3/file5", type}, regular},
	 {{"/root/dir/sub", type}, directory},
 	 {{"/root/dir/sub2", type}, directory},
 	 {{"/root/dir/sub2/sub3", type}, directory},
	 {{"/root/dir/sub", directory_content}, ["file3"]},
	 {{"/root/dir/sub2", directory_content}, ["sub3", "file4"]},
	 {{"/root/dir/sub2/sub3", directory_content}, ["file5"]}],
	["/root/dir/file",
	 "/root/dir/file2",
	 "/root/dir/sub/file3",
	 "/root/dir/sub2/file4",
	 "/root/dir/sub2/sub3/file5",
	 "/root/dir/sub",
	 "/root/dir/sub2",
	 "/root/dir/sub2/sub3"]}]).

list_recursively_with_additional_info_test () ->
    ok.
