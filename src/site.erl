%%% Copyright (c) 2004 Dominic Williams, Nicolas Charpentier.
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

-module(site).
-export([generate_tree/1]).

generate_tree({index,Title,Intro,Index}) ->
    [{file,"index.html.fr",generate_index(fr,Title,Intro,Index)},
     {file,"index.html.en",generate_index(en,Title,Intro,Index)},
     {directory,"fr",generate_docs(fr,Index)},
     {directory,"en",generate_docs(en,Index)}].

generate_index(Lang,Title,Intro,{IndexTitle,_}) ->
    xhtml:generate({html,[{lang,atom_to_list(Lang)},{'xml:lang',atom_to_list(Lang)}],
		    [{head,[],
		      [{meta,[{'http-equiv',"content-type"},{content,"text/html; charset=utf-8"}],[]},
		       {title,[],[select(Lang,Title)]},
		       {link,[{rel,"stylesheet"},{type,"text/css"},{href,"dw.css"}],[]}]},
		     {body,[],
		      [{p,[],[select(Lang,Intro)]},
		      {h2,[],[select(Lang,IndexTitle)]}]}]}).

%    lists:flatten([doctype(),html([h(1,LangFun(Title)),p(LangFun(Intro))])]).

generate_docs(LangFun,{Title,[{_Section,Docs}|Rest]}) ->
    MakeFile = fun(Atom) -> {file,atom_to_list(Atom)++".html","<html />"} end,
    lists:flatten([[MakeFile(DocAtom) || {DocAtom,_} <- Docs] | generate_docs(LangFun,{Title,Rest})]);
generate_docs(_,{_Title,[]}) ->
    [].

select(_,{String}) ->
    String;
select(fr,{FrString,_}) ->
    FrString;
select(en,{_,EnString}) ->
    EnString.
