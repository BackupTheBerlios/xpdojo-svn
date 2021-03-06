%%% Copyright (c) 2004-2007 Dominic Williams, Nicolas Charpentier
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

-module(file_monitor_acceptance).
-compile(export_all).
-import(testing,
	[receive_one_from/1,
	 purge_messages/1]).

use_and_purge_tree(Tree, Fun) ->
    testing:use_and_purge_tree_with_file_system(Tree, Fun).

notify() ->
    Self = self(),
    fun(Event, File_name, _) ->
	    Self ! {self(), {Event, File_name}}
    end.

missing_directory_test() ->
    use_and_purge_tree(
      [],
      fun (Dir, F) ->
	      File_name = filename:join(Dir,"nonexistent"),
	      Pid = file_monitor:start (F, File_name, notify()),
	      {nonexistent, File_name} = receive_one_from (Pid),
	      timeout = receive_one_from (Pid),
	      false = is_process_alive (Pid)
      end).

single_directory_test() ->
    use_and_purge_tree (
      [{directory,"foo",[]}],
      fun (Dir, F) ->
	      Directory = filename:join (Dir, "foo"),
	      Pid = file_monitor:start (F, Directory, notify()),
	      timeout = receive_one_from (Pid),
	      File_name = filename:join (Directory, "foo.txt"),
	      file:write_file(File_name,"Hello"),
	      {1, {found, File_name}} = {1, receive_one_from(Pid)},
	      ok = file:delete (File_name),
	      {2, {deleted, File_name}} = {2, receive_one_from(Pid)},
	      ok = file:del_dir (Directory),
	      {3, {nonexistent, Directory}} = {3, receive_one_from (Pid)},
	      {4,timeout} = {4,receive_one_from(Pid)},
	      file_monitor:stop(Pid),
	      timer:sleep(1000),
	      false = is_process_alive (Pid)
      end).

complex_test() ->
    use_and_purge_tree (
      [{file, "f1", ""},
       {directory, "d1", [{file, "d1f1", ""}]}],
      fun (Dir, F) ->
	      Pid = file_monitor:start (F, Dir, notify()),
	      F1 = filename:join (Dir, "f1"),
	      {found, F1} = receive_one_from(Pid),
	      D1F1 = filename:join ([Dir, "d1", "d1f1"]),
	      {found, D1F1} = receive_one_from(Pid),
	      timeout = receive_one_from(Pid),
	      file_monitor:stop(Pid)
      end).

stop_test() ->
    use_and_purge_tree (
      [{file, "f1", ""},
       {directory, "d1", [{file, "d1f1", ""}]}],
      fun (Dir,  F) ->
	      Pid = file_monitor:start (F, Dir, notify()),
	      purge_messages(5000),
	      file_monitor:stop(Pid),
	      timer:sleep(1000),
	      false = is_process_alive (Pid),
	      Directory = filename:join (Dir, "d1"),
	      File_name = filename:join (Directory, "d1f1"),
	      file:write_file (File_name, "Hello"),
	      timeout = receive_one_from(Pid)
      end).

revival_test() ->
    use_and_purge_tree (	      
      [{file,"myfile.txt","Hello"}],
      fun (Dir, F) ->
	      File_name = filename:join (Dir, "myfile.txt"),
	      Pid = file_monitor:start (F, Dir, notify()),
	      {found, File_name} = receive_one_from(Pid),
	      file:delete (File_name),
	      {deleted, File_name} = receive_one_from(Pid),
	      file:write_file (File_name, "Goodbye"),
	      {found, File_name} = receive_one_from(Pid),
	      file_monitor:stop(Pid)
      end).
    
crash_test() ->
    use_and_purge_tree (
      [{file, "f1", ""},
       {directory, "d1", [{file, "d1f1", ""}]}],
      fun (Dir, F) ->
	      io:fwrite("crash test: ~p~n",[F]),
	      Previous_processes = processes(),
	      Pid = file_monitor:start (F, Dir, notify()),
	      purge_messages(5000),
	      exit (Pid, kill),
	      timer:sleep(1000),
	      false = is_process_alive (Pid),
	      same_elements = adlib:compare (Previous_processes, processes())
      end).
    
directory_change_test() ->
    use_and_purge_tree (
      [{file, "f1", ""},
       {directory, "d1", [{file, "d1f1", ""}]}],
      fun (Dir, F) ->
	      {ok,Cwd} = file:get_cwd(),
	      file:set_cwd(Dir),
	      Pid = file_monitor:start (F, ".", notify()),
	      purge_messages(5000),
	      file:set_cwd("/"),
	      timer:sleep(3000),
	      true = is_process_alive (Pid),
	      file_monitor:stop(Pid),
	      file:set_cwd(Cwd)
      end).

modified_test() ->
    use_and_purge_tree (
      [{directory,"foo",[]}],
      fun (Dir, F) ->
	      Pid = file_monitor:start (F, Dir, notify()),
	      File_name = filename:join ([Dir, "foo", "foo.txt"]),
	      file:write_file (File_name,"Hello"),
	      {1, {found, File_name}} = {1, receive_one_from (Pid)},
	      file:write_file (File_name,"Goodbye"),
	      {2, {modified, File_name}} = {2, receive_one_from (Pid)},
	      ok = file:delete (File_name),
	      {3, {deleted, File_name}} = {3, receive_one_from(Pid)},
	      file_monitor:stop(Pid)
      end).

with_content_test() ->
    use_and_purge_tree (
      [{directory,"foo",[]}],
      fun (Dir, F) ->
	      Pid = file_monitor:start (F, Dir, file_monitor:bind_content(notify())),
	      File_name = filename:join ([Dir, "foo", "foo.txt"]),
	      file:write_file (File_name,"Hello"),
	      {1, {{found, "Hello"},File_name}} = {1, receive_one_from (Pid)},
	      file:write_file (File_name,"Goodbye"),
	      {2, {{modified, "Goodbye"}, File_name}} = {2, receive_one_from (Pid)},
	      ok = file:delete (File_name),
	      {3, {deleted, File_name}} = {3, receive_one_from(Pid)},
	      file_monitor:stop(Pid)
      end).

