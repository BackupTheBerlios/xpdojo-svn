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

-module(adlib).
-export([guarded_call/4,make_tree/2,delete_tree/1,use_tree/3,temporary_module_name/0,temporary_pathname/0]).
-export([first/2,unique/1,strip_whitespace/1,begins_with/2,begins_with/1,ends_with/2,ends_with/1,fold_files/4]).
-export([accumulate_if/3, is_below_directory/2]).

-include_lib("kernel/include/file.hrl").

guarded_call(Context, Setup, Function, Teardown) ->
    NewContext = Setup({context,Context}),
    case catch Function({context,NewContext}) of
	{'EXIT',Reason} ->
	    Teardown({{context,NewContext},noresult}),
	    throw({'EXIT',Reason});
	Result ->
	    LastContext = Teardown({{context,NewContext},{result,Result}}),
	    {result,Result,context,LastContext}
    end.

first(Pred,List) ->
						% Looks for first element of List satisfying Pred.
						% Returns {ok, {Element,Position}} | none
    first(Pred,List,1).

first(Pred,[H|T],Position) ->
    case Pred(H) of
	true ->
	    {ok, {H,Position}};
	false ->
	    first(Pred,T,Position+1)
    end;
first(_,[],_) ->
    none.


make_tree(Root,Tree) ->
    ok = file:make_dir(Root),
    populate(Root,Tree).

delete_tree(Root) ->
    depopulate(Root),
    ok = file:del_dir(Root).

use_tree(Dir,Tree,Fun) ->
    adlib:make_tree(Dir,Tree),
    LastCall = case catch Fun(Dir,Tree) of
		   {'EXIT',Reason} ->
		       fun() -> exit(Reason) end;
		   Result ->
		       fun() -> Result end
	       end,
    adlib:delete_tree(Dir),
    LastCall().

temporary_pathname() ->
    Possible_roots = [os:getenv(X) || X <- ["TMP","TEMP","HOME"], os:getenv(X)/=false],
    {ok, {Root,_}} = first(
		       fun(X) ->
			       case file:read_file_info(X) of
				   {ok,{file_info,_,directory,read_write,_,_,_,_,_,_,_,_,_,_}} ->
				       true;
				   _Other ->
				       false
			       end
		       end,
		       Possible_roots),
    Pathname = filename:join(Root,unique_string()),
    {error,enoent} = file:read_file_info(Pathname),
    Pathname.

unique_string() ->
    {Mega,Sec,Micro} = now(),
    integer_to_list(Mega) ++ "_" ++ integer_to_list(Sec) ++ "_" ++ integer_to_list(Micro).

temporary_module_name() ->
    list_to_atom("tmp_module_" ++ unique_string()).

populate(Directory, [{file,Name,Content}|Tail]) ->
    ok = file:write_file(filename:join(Directory,Name),normalise(Content)),
    populate(Directory,Tail);
populate(Directory, [{directory,Name,Content}|Tail]) ->
    Pathname = filename:join(Directory,Name),
    ok = file:make_dir(Pathname),
    populate(Pathname,Content),
    populate(Directory,Tail);
populate(_,[]) ->
    ok.

normalise([H|[]]) ->
    H;
normalise([H|T]) when list(H) ->
						% Inserts newlines when list of strings...
    normalise([string:concat(H,string:concat("\n",hd(T)))|tl(T)]);
normalise([String]) when list(String) ->
    String;
normalise(String) when list(String) ->
    String.

depopulate(Directory) ->
    {ok, Filename_list} = file:list_dir(Directory),
    Delete = fun(Filename) ->
		     Pathname = filename:join(Directory,Filename),
		     {ok,File_info} = file:read_file_info(Pathname),
		     case File_info#file_info.type of
			 directory ->
			     delete_tree(Pathname);
			 regular ->
			     ok = file:delete(Pathname)
		     end
	     end,
    lists:foreach(Delete,Filename_list).

unique(List) ->
    lists:reverse(
      lists:foldl(
        fun(X,Acc) ->
                accumulate_unless(lists:member(X,Acc),X,Acc)
        end,
        [],
        List)).

accumulate_unless(true, _X, Acc) ->
    Acc;
accumulate_unless(false, X, Acc) ->
    [X|Acc].

strip_whitespace(String) when list(String) ->
    lists:filter(
      fun($\s) -> false;
	 ($\n) -> false;
	 ($\t) -> false;
	 (Other) when integer(Other) -> true
      end,
      String).

ends_with(Atom, Ending) when atom(Atom) ->
    ends_with (atom_to_list(Atom), Ending);
ends_with(String,Ending) ->
    begins_with(lists:reverse(String),lists:reverse(Ending)).

begins_with (Atom, String) when atom(Atom) ->
    begins_with (atom_to_list (Atom), String);
begins_with([Char|StringTail],[Char|TokenTail]) ->
    begins_with(StringTail,TokenTail);
begins_with([Char1|_],[Char2|_]) when Char1 /= Char2 ->
    false;
begins_with([_Char|_],[]) ->
    true;
begins_with([],[_Char|_]) ->
    false;
begins_with([],[]) ->
    true.

ends_with(Ending) ->
    fun(String) ->
	    ends_with(String,Ending)
    end.

begins_with(Token) ->
    fun(String) ->
	    begins_with(String,Token)
    end.

fold_files(Root,Action,Options,Acc) when list(Root) ->
    {ok,Content} = file:list_dir(Root),
    lists:foldl(fun(Item,Acc2) ->
			[Type] = xray(Root,Item,[type],[]),
			fold_files(Type,Root,Item,Options,Action,Acc2)
		end,
		Acc,
		Content).

fold_files(directory,Root,Item,Options,Action,Acc) ->
    fold_files(filename:join(Root,Item),Action,Options,
	       Action(xray(Root,Item,Options,[]),Acc));
fold_files(_Type,Root,Item,Options,Action,Acc) ->
    Action(xray(Root,Item,Options,[]),Acc).

xray(Root,Item,[type|T],Acc) ->
    {ok,File_info} = file:read_file_info(filename:join(Root,Item)),
    xray(Root,Item,T,[File_info#file_info.type|Acc]);
xray(Root,Item,[relative_full_name|T],Acc) ->
    xray(Root,Item,T,[Item|Acc]);
xray(Root,Item,[absolute_full_name|T],Acc) ->
    xray(Root,Item,T,[filename:join(Root,Item)|Acc]);
xray(Root,Item,[extension|T],Acc) ->
    xray(Root,Item,T,[filename:extension(Item)|Acc]);
xray(_,_,[],Acc) ->
    lists:reverse(Acc).
    
accumulate_if (Item, List, true) ->
    [Item|List];
accumulate_if (_, List, false) ->
    List.

is_below_directory (Path1, Path2) ->
    is_below_directory2 (lists:reverse (filename:split (Path1)), lists:reverse (filename:split(Path2))).

is_below_directory2 ([], Path2) ->
    false;
is_below_directory2 (Path1, Path2)  when Path1 == Path2 ->
    true;
is_below_directory2 (Path1, Path2) ->
    is_below_directory2 (tl (Path1), Path2).
