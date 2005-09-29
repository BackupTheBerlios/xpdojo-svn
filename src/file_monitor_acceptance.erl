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

-module(file_monitor_acceptance).
-compile(export_all).
-import(testing,
	[use_and_purge_tree/2,
	 receive_one_from/1,
	 wait_for_detectable_modification_time/0,
	 purge_messages/1,
	 receive_one/0]).

notify() ->
    Self = self(),
    fun(Event, File_name) ->
	    Self ! {self(), {Event, File_name}}
    end.

missing_file_test() ->
    use_and_purge_tree (
      [],
      fun (Dir, _) ->
	      File_name = filename:join (Dir, "myfile.erl"),
	      Pid = file_monitor:start (File_name, notify()),
	      {nonexistent, File_name} = receive_one_from(Pid),
	      false = is_process_alive (Pid)
      end).

single_file_test() ->
    use_and_purge_tree (	      
      [{file,"myfile.txt","Hello"}],
      fun (Dir, _) ->
	      File_name = filename:join (Dir, "myfile.txt"),
	      Pid = file_monitor:start (File_name, notify()),
	      {found, File_name} = receive_one_from(Pid),
	      timeout = receive_one_from(Pid),
	      wait_for_detectable_modification_time(),
	      file:write_file (File_name, "Goodbye"),
	      {modified, File_name} = receive_one_from(Pid),
	      file:delete (File_name),
	      {deleted, File_name} = receive_one_from(Pid),
	      false = is_process_alive (Pid)
      end).
    
missing_directory_test() ->
    use_and_purge_tree(
      [],
      fun (Dir, _) ->
	      File_name = filename:join(Dir,"nonexistent"),
	      Pid = file_monitor:start (File_name, notify()),
	      {nonexistent, File_name} = receive_one_from (Pid),
	      timeout = receive_one_from (Pid),
	      false = is_process_alive (Pid)
      end).

single_directory_test() ->
    use_and_purge_tree (
      [{directory,"foo",[]}],
      fun(Dir,_) ->
	      Directory = filename:join (Dir, "foo"),
	      Pid = file_monitor:start (Directory, notify()),
	      {found, Directory} = receive_one_from(Pid),
	      timeout = receive_one_from (Pid),
	      File_name = filename:join (Directory, "foo.txt"),
	      file:write_file(File_name,"Hello"),
	      {modified, Directory} = receive_one_from(Pid),
	      {NewPid, {found, File_name}} = receive_one(),
	      ok = file:delete (File_name),
	      {modified, Directory} = receive_one_from(Pid),
	      {NewPid, {deleted, File_name}} = receive_one(),
	      ok = file:del_dir (Directory),
	      {deleted, Directory} = receive_one_from(Pid)
      end).

complex_test() ->
    use_and_purge_tree (
      [{file, "f1", ""},
       {directory, "d1", [{file, "d1f1", ""}]}],
      fun (Dir, _) ->
	      Pid = file_monitor:start (Dir, notify()),
	      {found, Dir} = receive_one_from (Pid),
	      F1 = filename:join (Dir, "f1"),
	      {_, {found, F1}} = receive_one(),
	      D1 = filename:join (Dir, "d1"),
	      {_, {found, D1}} = receive_one(),
	      D1F1 = filename:join ([Dir, "d1", "d1f1"]),
	      {_, {found, D1F1}} = receive_one(),
	      timeout = receive_one()
      end).


stop_test() ->
    use_and_purge_tree (
      [{file, "f1", ""},
       {directory, "d1", [{file, "d1f1", ""}]}],
      fun (Dir, _) ->
	      Pid = file_monitor:start (Dir, notify()),
	      purge_messages(5000),
	      
	      file_monitor:stop(Pid),
	      timer:sleep(1000),
	      Directory = filename:join (Dir, "d1"),
	      File_name = filename:join (Directory, "d1f1"),
	      file:write_file (File_name, "Hello"),
	      timeout = receive_one()
      end).
