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

-module(site_ut).
-compile(export_all).

empty_site() ->
    {index,{"Site"},{"Intro"},{{"Index"},[]}}.

tiny_site() ->
    {index,{"Mon Site", "My Site"},
     {"Intro au site","Site intro"},
     {{"Contenu","Content"},
      [{{"Section 1"},
	[{doc1,{"Doc 1"}}]},
       {{"Autre section","Other section"},
	[{doc2,{"Deuxième doc","Second doc"}},
	 {doc3,{"Dernier","Last"}}]}]}}.

generate_empty_test() ->
    [{file,"index.html.fr",Index_fr},
     {file,"index.html.en",Index_en},
     {directory,"fr",[]},
     {directory,"en",[]}] = site:generate_tree(empty_site()),
    pass = check_index(Index_fr,"fr","Site","Intro","Index"),
    pass = check_index(Index_en,"en","Site","Intro","Index").


check_index(Index,Lang,WinTitle,Intro,PageTitle) ->
    true = xhtml:has_doctype(Index),
    Tree = xhtml:parse(lists:flatten(Index)),
    WinTitle = xhtml:title(Tree),
    Lang = xhtml:language(Tree),
    "text/html; charset=utf-8" = xhtml:content_type(Tree),
    "dw.css" = xhtml:stylesheet(Tree),
    [{p,[],[Intro]}] = xhtml:find_elements(Tree,[html,body,p]),
    [{h2,[],[PageTitle]}] = xhtml:find_elements(Tree,[html,body,h2]),
    pass.


generate_tiny_test() ->
    [{file,"index.html.fr",Index_fr},
     {file,"index.html.en",Index_en},
     {directory,"fr",[{file,"doc1.html",_},
		      {file,"doc2.html",_},
		      {file,"doc3.html",_}]},
     {directory,"en",[{file,"doc1.html",_},
		      {file,"doc2.html",_},
		      {file,"doc3.html",_}]}] = site:generate_tree(tiny_site()),
    pass = check_index(Index_fr,"fr","Mon Site","Intro au site","Contenu"),
    pass = check_index(Index_en,"en","My Site","Site intro","Content").
    
