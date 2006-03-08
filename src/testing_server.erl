%%% Copyright (c) Dominic Williams, Nicolas Charpentier, Virgile Delecolle, 
%%% Fabrice Nourisson, Jacques Couvreur.
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

-module(testing_server).

-export([start/3,stop/0,loop/4]).

start(Dir,Notification,Options) ->
    Pid = spawn (?MODULE, loop, [Dir, Notification, Options, init]),
    register(testing_server,Pid),
    Pid.

stop()->
    testing_server ! stop.

loop (Dir, Notification, Options, State) ->
    New_state = xpdojo:test_files (Dir, Options),
	notify (Notification, New_state, State),
    receive
	stop ->
	    ok
    after 1000 ->
	    loop (Dir, Notification, Options, New_state)
    end.

notify (_, unchanged, _) ->
    ok;
notify (_, State, State) ->
    ok;
notify (Notify, New_state, _) ->
    Notify (New_state).
