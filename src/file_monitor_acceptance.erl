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
-import(testing, [use_and_purge_tree/2, receive_one_from/1, wait_for_detectable_modification_time/0]).

missing_file_test() ->
    use_and_purge_tree (
      [],
      fun (Dir, _) ->
	      File_name = filename:join (Dir, "myfile.erl"),
	      Pid = file_monitor:start (File_name, self()),
	      missing = receive_one_from(Pid),
	      false = is_process_alive (Pid)
      end).

single_file_test() ->
    use_and_purge_tree (	      
      [{file,"myfile.txt","Hello"}],
      fun (Dir, _) ->
	      File_name = filename:join (Dir, "myfile.txt"),
	      Pid = file_monitor:start (File_name, self()),
	      found = receive_one_from(Pid),
	      timeout = receive_one_from(Pid),
	      wait_for_detectable_modification_time(),
	      file:write_file (File_name, "Goodbye"),
	      modified = receive_one_from(Pid),
	      file:delete (File_name),
	      deleted = receive_one_from(Pid),
	      false = is_process_alive (Pid)
      end).
    
