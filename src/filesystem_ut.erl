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

tree() ->
    [{file, "toto", ["Hello"]},
     {directory, "tmp", [{file, "intmp", []}]},
     {file, "other.xml", ["<a>coucou</a>"]}].
    
commands_test() ->
    lists:foreach (
      fun (Test) ->
	      use_and_purge_tree (
		tree(),
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
       fun multiple_requests/2,
       modification_time (erlang:localtime ()),
       fun md5/2]).

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
    {Filesystem, Filename, Result} = receive_one (),
    true = adlib:contains ({error, enoent}, Result).
    
directory_content (Filesystem, Dir) ->
    Filesystem ! {self(), Dir, [directory_content]},
    {Filesystem, Dir,
     [{directory_content, List}]} = receive_one (),
    same_elements = adlib:compare (["other.xml", "tmp", "toto"], List).

md5 (Filesystem, Dir) ->
    Filename = filename:join (Dir, "toto"),
    Filesystem ! {self(), Filename, [md5]},
    {Filesystem, Filename,
     [{md5, <<139,26,153,83,196,97,18,150,168,39,171,248,196,120,4,215>>}]}  = receive_one ().

enotdir (Filesystem, Dir) ->
    Filename = filename:join (Dir, "toto"),
    Filesystem ! {self(), Filename, [directory_content]},
    {Filesystem, Filename, Result} = receive_one (),
    true = adlib:contains ({error, enotdir}, Result).

modification_time (Before) ->
    fun (Filesystem, Dir) ->
	    Filesystem ! {self(), Dir, [modification_time]},
	    {Filesystem, Dir, [{modification_time, Time}]} = receive_one(),
	    {1, Before, Time, true} = {1, Before, Time, Before =< Time},
	    After = erlang:localtime (),
	    {2, Time, After, true} = {2, Time, After, Time =< After}
    end.

content_test () ->
    use_and_purge_tree (
      tree (),
      fun (Dir, _) ->
	      Hello_file = filename:join (Dir, "toto"),
	      XML_file = filename:join (Dir, "other.xml"),
	      filesystem:serve(
		fun(F) ->
			F ! {self(), Hello_file, [content]},
			{F, Hello_file, [{content, "Hello"}]} = receive_one(),
			F ! {self(), XML_file, [content]},
			{F, XML_file, [{content, "<a>coucou</a>"}]} = receive_one()
		end)
      end).

multiple_requests (File_system, Dir) ->
    Filename = filename:join (Dir, "toto"),
    File_system ! {self(), Filename, [type, modification_time]},
    {File_system, Filename, [{type, regular}, {modification_time, {{_,_,_},{_,_,_}}}]} = receive_one ().

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

fake_file_system (Instructions) ->
    receive
	{Client, Path, [Command]} ->
	    case dict:find ({Path, Command}, Instructions) of
		{ok, Result} ->
		    Client ! {self(), Path, [{Command, Result}]};
		error ->
		    Client ! {self(), Path, [{Command, fake_has_no_instructions}]}
	    end,
	    fake_file_system (Instructions);
	stop ->
	    bye;
	Other ->
	    io:fwrite("fake_file_system received: ~p~n",[Other])
    end.

time1() ->
    {{2006, 3, 2}, {22, 30, 00}}.

fake_tree () ->
    [{"/root/dir", directory, time1(),
      [{"file", regular, time1()},
       {"file2", regular, time1()},
       {"sub", directory, time1(),
	[{"file3", regular, time1()}]},
       {"sub2", directory, time1(),
	[{"file4", regular, time1()},
	 {"sub3", directory, time1(),
	 [{"file5", regular, time1()}]}]}]}].

list_recursively_enoent_test() ->
    Instructions =
	[{{"/tmp", directory_content}, {error, enoent}}],
    F = spawn_link (?MODULE, fake_file_system, [dict:from_list (Instructions)]),
    {error, enoent} = filesystem:list_recursively (F, "/tmp"),
    F ! stop.
    
list_recursively_empty_test () ->
    F = testing:file_system ([{"/dev/null", directory, time, []}]),
    [] = filesystem:list_recursively (F, "/dev/null"),
    F ! stop.

list_recursively_test () ->
    F = testing:file_system (fake_tree()),
    Result = filesystem:list_recursively (F, "/root/dir"),
    F ! stop,
    Expected_result =
	["/root/dir/file",
	 "/root/dir/file2",
	 "/root/dir/sub/file3",
	 "/root/dir/sub2/file4",
	 "/root/dir/sub2/sub3/file5",
	 "/root/dir/sub",
	 "/root/dir/sub2",
	 "/root/dir/sub2/sub3"],
    same_elements = adlib:compare (Expected_result, Result).

list_recursively_with_additional_info_test () ->
    F = testing:file_system (fake_tree()),
    Result = filesystem:list_recursively (F, "/root/dir/sub2", [type]),
    F ! stop,
    Expected_result =
	[{"/root/dir/sub2/file4", regular},
	 {"/root/dir/sub2/sub3", directory},
	 {"/root/dir/sub2/sub3/file5", regular}],
    same_elements = adlib:compare (Expected_result, Result).    

list_recursively_with_bad_link_test () ->
    F = spawn (?MODULE, bad_link_file_system_loop, []),
    Result = filesystem:list_recursively (F, "/dir"),
    F ! stop,
    Expected_result =
	["/dir/foo", "/dir/sub", "/dir/sub/bar"],
    same_elements = adlib:compare (Expected_result, Result).

bad_link_file_system_loop () ->    
    receive
	{Pid, "/dir", [directory_content]} ->
	    Pid ! {self(), "/dir", [{directory_content, ["foo", "bad_link", "sub"]}]},
	    bad_link_file_system_loop();
	 {Pid, "/dir/foo", [type]} ->
	    Pid ! {self(), "/dir/foo", [{type, regular}]},
	    bad_link_file_system_loop();
	 {Pid, "/dir/bad_link", [type]} ->
	    Pid ! {self(), "/dir/bad_link", {error, enoent}},
	    bad_link_file_system_loop();
	 {Pid, "/dir/sub", [type]} ->
	    Pid ! {self(), "/dir/sub", [{type, directory}]},
	    bad_link_file_system_loop();
	{Pid, "/dir/sub", [directory_content]} ->
	    Pid ! {self(), "/dir/sub", [{directory_content, ["bar"]}]},
	    bad_link_file_system_loop();
	 {Pid, "/dir/sub/bar", [type]} ->
	    Pid ! {self(), "/dir/sub/bar", [{type, regular}]},
	    bad_link_file_system_loop();
	stop ->
	    bye
    end.
