%%% copyright (c) 2005 Nicolas Charpentier
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
-module(green_bar_acceptance).
-compile(export_all).
-import(testing,
        [use_and_purge_tree/2,
         purge_messages/1]).

start_green_bar_test() ->
    use_and_purge_tree (
      [],
      fun (_Dir, _) ->
              Pid = green_bar:start (),
              true = is_process_alive (Pid),
                                                % Hack before next Test
              exit (Pid, kill)
      end).

distant_start_green_bar_test() ->
    use_and_purge_tree (
      [],
      fun (_Dir, _) ->
              Pid = green_bar:start (node()),
              true = is_process_alive (Pid),
                                                % Hack before next Test
              exit (Pid, kill)
      end).
    
stop_green_bar_test() ->
    use_and_purge_tree (
      [],
      fun (_Dir, _) ->
              _Previous_processes = processes(),
              Pid = green_bar:start (),
              true = is_process_alive (Pid),
              green_bar:stop(Pid),
              ok
      end).

kill_green_bar_test() ->
    use_and_purge_tree (
      [],
      fun (_Dir, _) ->
              _Previous_processes = processes(),
              Pid = green_bar:start (),
              purge_messages(3000),
              exit (Pid, kill),
              false = is_process_alive (Pid),
              ok
      end).

components_green_bar_test() ->
    use_and_purge_tree (
      [],
      fun (_Dir, _) ->
              Pid = green_bar:start (),
              Components = green_bar:components(Pid),
              true = adlib:contains(acceptance,Components),
              true = adlib:contains(unit,Components),
              true = adlib:contains(modules,Components),
              green_bar:stop(Pid)
      end).

red_color_by_defaut_test() ->
    Pid = green_bar:start (),
    adlib:use_tree (
      adlib:temporary_pathname(),
      [],
      fun (_Dir, _) ->
              Components = green_bar:components(Pid),
              GraphicalBar = [X || {A,X} <- Components, case A of acceptance ->true; unit-> true; modules-> true;_Other-> false end],
              Assert = fun(Button) -> red = gs:read(Button,bg) end,
              lists:foreach(Assert,GraphicalBar)
      end,
      fun(_Dir,_) ->
              purge_messages(0),
              green_bar:stop(Pid)
      end).

reporting_message_test() ->
    Pid = green_bar:start (),
    adlib:use_tree (
      adlib:temporary_pathname(),
      [],
      fun (_Dir, _) ->
              GraphicalBar = [X || {A,X} <- green_bar:components(Pid), case A of acceptance ->true; unit-> true; modules-> true;_Other-> false end],
              GoodMessage = [{acceptance,3,3},{modules,1,1},{unit,0,0}],
              green_bar:notify(Pid,GoodMessage),
              timer:sleep(1000),
              AssertGreen = fun(Button) -> green = gs:read(Button,bg) end,
              lists:foreach(AssertGreen,GraphicalBar),
              WrongMessage = [{acceptance,9,3},{modules,0,1},{unit,"aa",0}],
              green_bar:notify(Pid,WrongMessage),
              timer:sleep(1000),
              AssertRed = fun(Button) -> red = gs:read(Button,bg) end,
              lists:foreach(AssertRed,GraphicalBar)
      end,
      fun(_Dir,_) ->
              purge_messages(0),
              green_bar:stop(Pid)
      end).
   
