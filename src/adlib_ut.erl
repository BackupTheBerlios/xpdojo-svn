%%% Copyright (c) Dominic Williams, Nicolas Charpentier, Virgile Delecolle.
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

-module(adlib_ut).
-compile(export_all).

-include_lib("kernel/include/file.hrl").

temporary_pathname_test() ->
	Name = adlib:temporary_pathname(),
	true = is_list(Name),
	{error,enoent} = file:read_file_info(Name),
	Next_name = adlib:temporary_pathname(),
	false = string:equal(Name,Next_name),
	ok = file:make_dir(Name),
	ok = file:del_dir(Name),
	ok = file:write_file(Next_name,list_to_binary("Hello world")),
	ok = file:delete(Next_name),
	pass.

temporary_module_name_test() ->
	Name = adlib:temporary_module_name(),
	true = is_atom(Name),
	false = code:is_loaded(Name),
	non_existing = code:which(Name),
	Next_name = adlib:temporary_module_name(),
	false = string:equal(Name,Next_name),
	pass.
	
tree() ->
    [{file,"test","Hello world"},
     {file,"test2",["Goodbye","world!"]},
     {directory,"testdir",[{file,"toto","Silly"},
			   {directory,"testdir2",[{file,"titi","Very silly"}]}]}].
    
tree_handling_test() ->
    Tmp_dirname = adlib:temporary_pathname(),
    Tree = tree(),
    adlib:make_tree(Tmp_dirname,Tree),
    {ok,Content} = file:read_file(filename:join(Tmp_dirname,"test")),
    "Hello world" = binary_to_list(Content),
    {ok,Content2} = file:read_file(filename:join(Tmp_dirname,"test2")),
    "Goodbye\nworld!" = binary_to_list(Content2),
    {ok,Content3} = file:read_file(filename:join([Tmp_dirname,"testdir","toto"])),
    "Silly" = binary_to_list(Content3),
    {ok,Content4} = file:read_file(filename:join([Tmp_dirname,"testdir","testdir2","titi"])),
    "Very silly" = binary_to_list(Content4),
    adlib:delete_tree(Tmp_dirname),
    {error,enoent} = file:read_file_info(Tmp_dirname),
    pass.

use_tree_test() ->
    Tmp_dirname = adlib:temporary_pathname(),
    Tree = tree(),
    Fun = fun(_Dir,_Tree) -> ok
	  end,
    ok = adlib:use_tree(Tmp_dirname,Tree,Fun),
    {error,enoent} = file:read_file_info(Tmp_dirname),

    ReturnFilesfun = fun(Dir,UsedTree) ->
			     [ X || {_,X,_} <- UsedTree, filelib:is_file(filename:join(Dir,X))]
		     end,
    ["test","test2","testdir"] = adlib:use_tree(Tmp_dirname,Tree,ReturnFilesfun),
    {error,enoent} = file:read_file_info(Tmp_dirname),

    case catch adlib:use_tree(Tmp_dirname,Tree, fun_that_explodes()) of
	{'EXIT', suicide} ->
	    ok
    end,
    {error,enoent} = file:read_file_info(Tmp_dirname),
    ok.

fun_that_explodes () ->    
    fun (_, _) ->
	    exit (suicide)
    end.

use_tree_and_cleanup_test () ->
    Tmp_dirname = adlib:temporary_pathname (),
    put(cleanup_called, false),
    case catch adlib:use_tree (
		 Tmp_dirname,
		 tree(),
		 fun_that_explodes(),
		 fun (Dir, _) ->
			 put (cleanup_called, {true, Dir})
		 end) of
	{'EXIT', suicide} ->
	    ok
    end,
    {true, Tmp_dirname} = get (cleanup_called).
    
unique_test() ->
	[] = adlib:unique([]),
	Sequence = lists:seq(1,20),
	Sequence = adlib:unique(Sequence),
	[1,3,6,12] = adlib:unique([1,1,3,1,6,12,6,1]).

strip_whitespace_test() ->
	"hello" = adlib:strip_whitespace(" hello"),
	"hello" = adlib:strip_whitespace("\s\shello"),
	"hello" = adlib:strip_whitespace("\nhello"),
	"hello" = adlib:strip_whitespace("\thello"),
	"hello" = adlib:strip_whitespace("\t\n\s  \thello"),
	pass.
	
begins_with_test() ->
    true = adlib:begins_with("hello","hell"),
    true = adlib:begins_with("yo","yo"),
    false = adlib:begins_with("hello","lo"),
    false = adlib:begins_with("yo","longer"),
    true = adlib:begins_with(this_is_an_atom,"this_").

ends_with_test() ->
    true = adlib:ends_with("bla","a"),
    false = adlib:ends_with("hello","yo"),
    false = adlib:ends_with("yo","longer"),
    true = adlib:ends_with (this_is_an_atom, "tom").
    
ends_with_fun_test() ->
    Ends_with_suf = adlib:ends_with("_suf"),
    true = Ends_with_suf("blabla_suf"),
    false = Ends_with_suf("_sufyohoho").

begins_with_fun_test() ->
    Begins_with_bla = adlib:begins_with("bla"),
    true = Begins_with_bla("blarhubarb").
    
accumulate_if_test () ->
    [] = adlib:accumulate_if (false, foo, []),
    [foo, bar] = adlib:accumulate_if (true, foo, [bar]).

is_below_directory_test() ->
    true = adlib:is_below_directory ("/tmp", "/tmp"),
    false = adlib:is_below_directory ("/tmp", "/home/toto"),
    false = adlib:is_below_directory ("/tmpbla", "/tmp"),
    true = adlib:is_below_directory ("/Users/dodo/dir/my_file", "/Users").


options_test() ->
   [{titi, {1,2}}, {toto, "MYTOTO"}] = adlib:update_options ([{toto, "MYTOTO"}], [{titi, {1,2}}, {toto, "MONTOTO"}]),
   [{titi, {1,2}}] = adlib:update_options ([{toto, "MYTOTO"}], [{titi, {1,2}}]).

normalise_path_test() ->
	"Toto/titi" = adlib:normalise_path("Toto/titi"),
	"Toto/titi" = adlib:normalise_path("Toto/./titi"),
	"Foo/bar" = adlib:normalise_path ("./Bla/../Foo/bar/titi/..").

compare_test() ->
    same_elements = adlib:compare ([],[]),
    same_elements = adlib:compare ([1, a, "Hello"], [a, 1, "Hello"]),
    {{left_extras, [bla, 1]}, {right_extras, []}} = adlib:compare (["Wow", 1, bla], ["Wow"]),
    {{left_extras, [1]}, {right_extras, [2, "Yo"]}} = adlib:compare (["Wow", 1, bla], ["Wow", 2, "Yo", bla]).
    
contains_test() ->
    lists:foldl (
      fun ({TermToFind, TermToSearch, ExpectedResult}, Index) ->
	      {Index, ExpectedResult} = {Index, adlib:contains (TermToFind, TermToSearch)},
	      Index + 1
      end,
      1,
      [{ok, ok, true},
       {yes, no, false},
       {ok, {yes, ok}, true},
       {ok, {{yes, ok}}, true},
       {{atom1, 3, "hello"}, {4, [ok, {other_atom, 3, "hello"}, 6]}, false},
       {{atom1, 3, "hello"}, {4, [ok, {atom1, 3, "hello"}, 6]}, true},
       {[1, 2, 3], [{1, 2}, 3], false}
      ]).

