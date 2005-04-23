%%% Copyright (c) Dominic Williams, Nicolas Charpentier.
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

-module (compiling).
-export ([modules_from_directory/2, differences/2, differences/3, file_time/1, module_time/1]).
-export ([source_of_module/1, purge_modules_from_directory/1, loaded_modules/0, loaded_modules/1]).
-export ([compile/2]).
-export ([compile/3]).

-include_lib("kernel/include/file.hrl").

loaded_modules () ->
    [X || {X,_Y} <- code:all_loaded()].

purge_modules_from_directory (Dir) ->
    lists:foreach (fun(M) -> code:purge(M) end, loaded_modules (Dir)),
    lists:foreach (fun(M) -> code:delete(M) end, loaded_modules (Dir)),
    lists:foreach (fun(M) -> code:purge(M) end, loaded_modules (Dir)).
    
source_of_module (Atom) ->
    source_of_module2 (lists:keysearch (source, 1, Atom:module_info (compile))).

source_of_module2 ({value, {source, Source}}) ->
    Source;
source_of_module2 (false) ->
    unknown.

loaded_modules (Dir) ->
    modules_from_directory (loaded_modules(), Dir).

modules_from_directory (Modules, Directory) ->
    modules_from_directory(filename:pathtype(Directory),Modules,Directory).

modules_from_directory (relative,Modules, Directory) ->
    {ok, Current } = file:get_cwd(),
    RelativeDirectory = string:concat(string:concat(Current,"/"),Directory),
    lists:foldl (
      fun (Module, Acc) ->
 	      adlib:accumulate_if (adlib:is_below_directory (source_of_module (Module), RelativeDirectory), Module, Acc)
      end,
      [],
      Modules);
modules_from_directory (_,Modules, Directory) ->
     lists:foldl (
       fun (Module, Acc) ->
	       adlib:accumulate_if (adlib:is_below_directory (source_of_module (Module), Directory), Module, Acc)
       end,
       [],
       Modules).

module_time (Module) ->
    file_time (element (2, code:is_loaded (Module))).

file_time (File) ->
    {ok, File_info} = file:read_file_info(File),
    {{Y,M,D},{H,Min,S}} = File_info#file_info.mtime,
    {Y,M,D,H,Min,S}.

is_modified (Module, File) ->
    module_time (Module) < file_time (File).

differences (Modules, Files) ->
    differences (Modules, Files, fun is_modified/2).

differences (Modules, Files, Is_modified_fun) ->
    Sorted_modules = lists:sort (Modules),
    Files_with_module_key = [{module_name (X), X} || X <- Files],
    Files_sorted_by_module = lists:keysort (1, Files_with_module_key),
    differences2 (Sorted_modules, Files_sorted_by_module, [], [], [], Is_modified_fun).

differences2 ([Module | Modules_tail], [{Module, File} | Files_tail], Added, Deleted, Modified, Is_modified) ->
    differences2 (Modules_tail, Files_tail, Added, Deleted, adlib:accumulate_if (Is_modified (Module, File), File, Modified), Is_modified);

differences2 (Modules = [Module1 | _], [{Module2, File} | Files_tail], Added, Deleted, Modified, Is_modified) when Module1 > Module2 ->
    differences2 (Modules, Files_tail, [File | Added], Deleted, Modified, Is_modified);

differences2 ([Module1|Remaining_modules], Files=[{Module2,_}|_], Added, Deleted, Modified, Is_modified) when Module1 < Module2 ->
    differences2 (Remaining_modules, Files, Added, [Module1|Deleted], Modified, Is_modified);

differences2 ([], [{_Module, File} | T], Added, Deleted, Modified, Is_modified) ->
    differences2 ([], T, [File|Added], Deleted, Modified, Is_modified);

differences2 ([Module|T], [], Added, Deleted, Modified, Is_modified) ->
    differences2 (T, [], Added, [Module|Deleted], Modified, Is_modified);

differences2 ([], [], Added, Deleted, Modified, _) ->
    cleanup_accumulators ([{added, Added}, {deleted, Deleted}, {modified, Modified}], []).

cleanup_accumulators ([{_Atom, []} | T], Acc) ->
    cleanup_accumulators (T, Acc);

cleanup_accumulators ([{Atom, List} | T], Acc) ->
    cleanup_accumulators (T, [{Atom, lists:reverse (List)} | Acc]);

cleanup_accumulators ([], Acc) ->
    lists:reverse (Acc).

module_name (File) ->
    list_to_atom (filename:basename (File, ".erl")).

compile (Dir, List) ->
    compile (Dir, List, fun(X) -> io:fwrite("~p~n",[X]) end).
				
compile (Dir, List, Report_function) ->
    compile (Dir, List, Report_function, {[], []}).

compile (_Dir, [], _Fun, {Compiled, Failed}) ->
    {{compiled, Compiled}, {failed, Failed}};
compile (Dir, [File|T], Report_function, Acc) ->
    compile (Dir, T, Report_function,
	     classify_by_result (Report_function, compile:file(File, [return]), File, Acc)).

classify_by_result (Report_function, {ok, Module, Warnings}, File, {Compiled, Failed}) ->
    Report_function ({compile, {ok, File, Warnings}}),
    {[Module|Compiled], Failed};
classify_by_result (Report_function, {error,Errors,Warnings}, File, {Compiled, Failed}) ->
    Report_function ({compile, {error, File, Errors, Warnings}}),
    {Compiled, [module_name (File) | Failed]}.

