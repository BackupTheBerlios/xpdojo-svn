%%% Copyright (c) Dominic Williams.
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

-module (directory_tree).
-export ([changes/2]).

changes (A,A) ->
    [];
changes (A,B) ->
    changes (lists:keysort(1, A), lists:keysort(1, B), no_changes()).

changes([], [{File, _} | Tail], Acc) ->
    changes ([], Tail, append (found, File, Acc) );
changes ([{File, _} | Tail], [], Acc) ->
    changes (Tail, [], append (deleted, File, Acc));
changes([Head|Tail1], [Head|Tail2], Acc ) ->
    changes(Tail1, Tail2, Acc);
changes([{File, _} | Tail1], [{File, _} | Tail2], Acc) ->
    changes(Tail1, Tail2, append (modified, File, Acc));
changes (Tree1 = [{File1, _} | _], [{File2, _} | Tail2], Acc) when File1 > File2 ->
    changes (Tree1, Tail2, append (found, File2, Acc));
changes ([{File1, _} | Tail1], Tree2 = [{File2, _} | _], Acc) when File1 < File2 ->
    changes (Tail1, Tree2, append (deleted, File1, Acc));
changes ([], [], {Found, Modified, Deleted}) ->
    non_empty_results_from ([{deleted, Deleted}, {found, Found}, {modified, Modified}]).

non_empty_results_from (List) ->
    lists:filter (
      fun({_, []}) -> false;
	 (_) -> true
      end,
      List).

no_changes() ->
    {[], [], []}.

append (found, File, {Found, Modified, Deleted}) ->
    {[File|Found], Modified, Deleted};
append (deleted, File, {Found, Modified, Deleted}) ->
    {Found, Modified, [File|Deleted]};
append (modified, File, {Found, Modified, Deleted}) ->
    {Found, [File|Modified], Deleted}.
