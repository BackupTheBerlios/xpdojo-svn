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
-module(green_bar).
-export([start/0,start/1,stop/1,init/0,notify/2,components/1]).

start() -> 
    start(node()).

start(Node) ->
    spawn(Node,?MODULE,init,[]).

stop(Pid) ->
    Pid ! stop.

components(Pid) ->
    Pid ! {self(),components},
    receive
	{components,Components} ->
	    Components
    after 5000 ->
	    []
    end.	

notify(Pid,Message) ->
    Pid ! {self(), notify, Message }.

init() ->
    WH = [{width,200},{height,300}],
    Main = gs:window(gs:start(),[{map,true},{title,"XP Day Green Bar"},{configure,true}|WH]),
    gs:frame(packer,Main,[{packer_x,[{stretch,1}]},
			  {packer_y,[{stretch,1,50},{stretch,1,50},{stretch,1,50}]}]),
    Acceptance =  gs:button(packer,[{label,{text,"Acceptance"}},{bg,red},{pack_xy,{1,1}}]),
    Unit =  gs:button(packer,[{label,{text,"Unit"}},{bg,red},{pack_xy,{1,2}}]),
    Modules =  gs:button(packer,[{label,{text,"Module"}},{bg,red},{pack_xy,{1,3}}]),
    gs:config(packer,Main),
    loop([{main,Main},{acceptance,Acceptance},{unit,Unit},{modules,Modules}]).

loop(Components) ->
    receive
	{gs,_Id,configure,_Data,[W,H|_]} ->
            gs:config(packer,[{width,W},{height,H}]),
            loop(Components);
	stop ->
	    bye;
	{Requester,components} ->
	    Requester ! {components,Components},
            loop(Components);
	{_Requester,notify, Evolution} ->
	    update_components(Components,Evolution),
	    loop(Components);
	_ ->
	    bye
    end.

update_components(_Components,[]) ->
    ok;
update_components(Components,[{Type,Expected,Good}|Tail]) ->
    update_Color(lists:keysearch(Type,1, Components),Expected,Good),
    update_components(Components,Tail);
update_components(_Components,_) ->
    ok.

update_Color(false,_,_) ->
    io:fwrite("Not Found ~n",[]),
    ok;
update_Color({value,{_,Graphical}},X,X) ->
    gs:config(Graphical,[{bg,green}]);
update_Color({value,{_,Graphical}},_,_) ->
    gs:config(Graphical,[{bg,red}]).

