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

-module(testing).
-export([run_functions/1,run_modules/2,run_unit_tests/0,run_acceptance_tests/0,run_all/0]).

-import(lists).
-import(string).

run_functions(Functions) when list(Functions) ->
    lists:foldl(
      fun(Fun, {Count,Errors}) ->
	      case catch Fun() of
		  {'EXIT', Reason} ->
		      {Count+1,[Reason|Errors]};
		  _Other ->
		      {Count+1,Errors}
	      end
      end, {0,[]}, Functions).

run_modules(Modules,Pattern) when list(Modules) ->
    lists:foldl(
      fun(Module, {Count,Errors}) ->
	      {ModuleCount,ModuleErrors} = run_functions(select_test_functions(Module,Pattern)),
	      {Count+ModuleCount,lists:append(ModuleErrors,Errors)}
      end, {0,[]}, Modules);
run_modules(Pred,Pattern) ->
    Modules = [ X || {X,_} <- code:all_loaded(), Pred(X) ],
    run_modules(Modules,Pattern).

run_all() ->
    {{unit,run_unit_tests()},{acceptance,run_acceptance_tests()}}.

run_unit_tests() ->
    Filter = fun(Module) ->
		     adlib:ends_with(atom_to_list(Module),"_ut")
	     end,
    run_modules(Filter,{suffix,"_test"}).

run_acceptance_tests() ->
    Filter = fun(Module) ->
		     adlib:ends_with(atom_to_list(Module),"_cat")
	     end,
    run_modules(Filter,{suffix,"_cat"}).
		     
select_test_functions(Module,Pattern) when atom(Module), tuple(Pattern) ->
    [{Module,X} || {X,Y} <- Module:module_info(exports), is_match(X,Pattern), Y == 0].

is_match(Function,{prefix,Prefix}) ->		  
    Function_prefix = string:left(atom_to_list(Function),length(Prefix)),
    string:equal(Function_prefix,Prefix);
is_match(Function,{suffix,Suffix}) ->
    Function_suffix = string:right(atom_to_list(Function),length(Suffix)),
    string:equal(Function_suffix,Suffix).

