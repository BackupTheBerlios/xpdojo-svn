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

-module(xpdojo).

-export([dashboard/2]).

dashboard( [{files,[]}], _Atom) ->
    empty_project;
dashboard( [{directory,Name}], _Atom) ->
    KeepFiles = fun([regular,EntryName],Acc) ->
			[EntryName|Acc];
		   (_,Acc) ->
			Acc
		end,
    Files = adlib:fold_files(Name,KeepFiles,[type,absolute_full_name],[]),
    case Files of
	[] ->
	    empty_project;
	List when list(List) ->
	    dashboard([{files,Files}],_Atom)
    end;
dashboard( [{files,Files}], _Atom) ->
    ModuleCompilation = compileModules(Files),
    
    checkNotEmpty(ModuleCompilation,
		  lists:all(fun({_File,non_existent}) -> true;
			       (_X) ->false
			    end,ModuleCompilation)).

compileModules([]) ->
    [];
compileModules(Files) ->
    Fun = fun(F,Acc) ->
		  CompilationResult = compile:file(F,[binary,return_errors]),
		  [{F,transform_compilation_result(CompilationResult)}|Acc]
	  end,
    lists:foldl(Fun,[],Files).

checkNotEmpty([],_) ->
    empty_project;
checkNotEmpty(_,true) ->
    empty_project;
checkNotEmpty(Modules,false) ->
    checkCompilation(Modules,
		     lists:any(fun({_File,compilation_error}) -> true;
				  (_X) ->false
			       end,Modules)).
checkCompilation(_,true) ->
    build_failed;
checkCompilation(Modules,false) ->
    unitTestCompilation(Modules).

unitTestCompilation(Modules) ->
    Fun = fun({File,{compiled,Module,Binary}},Acc) ->
		  Corresponding_unit_test_file = string:concat(string:concat(string:substr(File,1,string:len(File)-4),"_ut"),
							       string:substr(File,string:len(File)-3,4)),
		  CompilationResult = compile:file(Corresponding_unit_test_file,[binary,return_errors]),
		  [{File,{module,Module,Binary},{unit_test,transform_compilation_result(CompilationResult)}}|Acc]
	  end,
    unitTestRun(lists:foldl(Fun,[],Modules)).
    
unitTestRun(Modules) ->
    Fun = fun({_File,{module,Module,Binary},
	       {unit_test,{compiled,
			   UnitTestModule,
			   UnitTestBinary
			  }
	       }
	      },Acc) ->
		  load_module_in_memory(Module,Binary),
		  load_module_in_memory(UnitTestModule,UnitTestBinary),
		  [UnitTestModule|Acc];
	     (_X,Acc) ->
		  Acc
	  end,
    UnitTestModules = lists:foldl(Fun,[],Modules),
    Result = testing:run_modules(UnitTestModules,{suffix,"_test"}),
    Result.

transform_compilation_result({error,[{_,[{none,compile,{epp,enoent}}]}],_}) ->
    non_existent;
transform_compilation_result({error,_A,_}) ->
    compilation_error;
transform_compilation_result({ok,Module,Binary}) ->
    {compiled,Module,Binary}.

load_module_in_memory(Module,Binary) ->
    unload_module(lists:member(Module,erlang:loaded()),Module),
    {module,Module} = erlang:load_module(Module,Binary),
    ok.
    
unload_module(true,Module) ->
    erlang:delete_module(Module),
    erlang:purge_module(Module),
    ok;
unload_module(false,_) ->
    ok.
