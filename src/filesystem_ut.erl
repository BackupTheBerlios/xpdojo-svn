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

-module(filesystem_ut).
-compile(export_all).
-import(testing, [use_and_purge_tree/2, receive_one/0]).

filesystem_test() ->
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
       fun enotdir/2]).

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

%%% multiple_requests_test () ->
%%%     ok = not_coded.
