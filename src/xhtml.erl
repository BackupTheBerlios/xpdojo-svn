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

-module(xhtml).
-export([attribute/3,generate/1,title/1,has_doctype/1,parse/1,tokenize/1,from_text/1,is_valid_tag/1,escape/1,doctype/0,find_elements/2,find_paths/2,language/1,stylesheet/1,content_type/1]).

%% Exported functions

escape(String) when list(String) ->
    lists:flatten([escape(Char) || Char <- String]);
escape($<) ->
    "&lt;";
escape($>) ->
    "&gt;";
escape($&) ->
    "&amp;";
escape(Char) ->
    Char.

from_text(String) ->
    lists:reverse(parse_text(String,[],newline)).

is_valid_tag(Tag) ->
    lists:member(Tag, lists:append(always_empty_tags(),usually_not_empty_tags())).

doctype() ->
    "<!DOCTYPE html\n"
	"PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\"\n"
	"\"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">\n".

has_doctype(String) ->
    case string:str(String,doctype()) of
	1 ->
	    true;
	_ ->
	    false
    end.

title({html,_,Content}) ->
    title(Content,html).

title([{head,_,Content}|_],html) ->
    title(Content,head);
title([{title,_,[Content]}|_],head) when list(Content), integer(hd(Content)) ->
    Content;
title([_|T],head) ->
    title(T,head).

language(Tree) ->
    Lang1 = attribute(Tree,html,lang),
    Lang2 = attribute(Tree,html,'xml:lang'),
    report_language(Lang1,Lang2).

stylesheet(Tree) ->
    [{link,[{rel,"stylesheet"},{type,"text/css"},{href, Stylesheet }],[]}] = xhtml:find_elements(Tree,[html,head,link]),
    Stylesheet.

content_type([{meta,[{'http-equiv',"content-type"},{content,Content_type}],[]}|_]) ->
    Content_type;
content_type([_|T]) ->
    content_type(T);
content_type([]) ->
    error;
content_type(Tree) ->
    content_type(xhtml:find_elements(Tree,[html,head,meta])).
    
report_language([Lang],[Lang]) ->
    Lang;
report_language(_,_) ->
    error.

generate(Tree) ->
    doctype()++generate(Tree,[]).

parse(String) ->
    parse(init, tokenize(String), {}).

tokenize(String) ->

    case string:str(String,doctype()) of
	1 ->
	    HeadLength = string:len(doctype()),
	    String_of_interest = string:substr(String,HeadLength+1),
	    tokenize({data,[]}, String_of_interest, [doctype]);
	0 ->
	    String_of_interest = String,
	    tokenize({data,[]}, String_of_interest, [])
    end.

attribute(Tree,Tag,Attribute_name) ->
    Paths = find_paths(Tree,Tag),
    Elements = lists:flatten([find_elements(Tree,Path)||Path<-Paths]),
    lists:foldl(fun({_,Attributes,_},Acc) ->
			case lists:keysearch(Attribute_name,1,Attributes) of
			    {value,{Attribute_name,Value}} ->
				[Value|Acc];
			    false ->
				Acc
			end
		end,
		[],
		Elements).

%% Sub-functions

generate({Tag,Attr,[String|T]}, Acc) when list(String) ->
    generate({Tag,Attr,T}, [String|Acc]);
generate({Tag,Attr,[Element|T]}, Acc) when tuple(Element) ->
    generate({Tag,Attr,T}, [generate(Element,[])|Acc]);
generate({Tag,Attr,[]}, Acc) ->
    markup(atom_to_list(Tag),Attr,lists:flatten(lists:reverse(Acc))).

parse(init, [doctype|T], {}) ->
    parse(init, T, {});
parse(init, [lt,html|T], {}) ->
    parse(in_markup_after_tag, T, {html,[],[],{}}); % {tag,attributes,elements,parent}
parse(in_markup_before_tag, [slash|T], Parent) ->
    parse(closing, T, Parent);
parse(in_markup_before_tag, [Tag|T], Parent) when atom(Tag) ->
    true = is_valid_tag(Tag),
    parse(in_markup_after_tag, T, {Tag,[],[],Parent});
parse(in_markup_after_tag, [gt|T], {Tag,Attr,[],Parent}) ->
    parse(content, T, {Tag,Attr,[],Parent});
parse(in_markup_after_tag, [slash|T], {Tag,Attr,[],Parent}) ->
    parse(closing, T, {Tag,Attr,[],Parent});
parse(in_markup_after_tag, [Attr_name|T], {Tag,Attr,[],Parent}) when atom(Attr_name) ->
    parse(in_markup_after_attr_name, T, {Tag,{Attr_name,Attr},[],Parent});
parse(in_markup_after_attr_name, [Attr_value|T], {Tag,{Attr_name,Attr},[], Parent}) when list(Attr_value) ->
    parse(in_markup_after_tag, T, {Tag,[{Attr_name,Attr_value}|Attr],[], Parent});
parse(content, [lt|T], Tree) ->
    parse(in_markup_before_tag, T, Tree);
parse(content, [String|T], {Tag,Attr,Content,Parent}) ->
    parse(content, T, {Tag,Attr,[String|Content],Parent});
parse(closing, [gt|T], {Tag,Attr,[],{OuterTag,OuterAttr,OuterContent,OuterParent}}) ->
    parse(content, T, {OuterTag,OuterAttr,[{Tag,lists:reverse(Attr),[]}|OuterContent],OuterParent});
parse(closing, [Tag|[gt|T]], {Tag,Attr,Content,{OuterTag,OuterAttr,OuterContent,OuterParent}}) when atom(Tag) ->
    parse(content, T, {OuterTag,OuterAttr,[{Tag,lists:reverse(Attr),lists:reverse(Content)}|OuterContent],OuterParent});
parse(closing, [Tag,gt], {Tag,Attr,Content,{}}) when atom(Tag) ->
    {Tag,lists:reverse(Attr),lists:reverse(Content)}.

tokenize({data,[]}, [H|T], Acc) when H==$\s; H==$\n; H==$\t; H==$\f; H==$\r ->
    tokenize({data,[]}, T, Acc);
tokenize({data,[]}, [$<|T], Acc) ->
    tokenize({{markup,tag},[]}, T, [lt|Acc]);
tokenize({data,Buffer}, [$<|T], Acc) ->
    tokenize({{markup,tag},[]}, T, [lt|[lists:reverse(Buffer)|Acc]]);
tokenize({{markup,Part},[]}, [H|T], Acc) when H==$\s; H==$\n; H==$\t; H==$\f; H==$\r ->
    tokenize({{markup,Part},[]}, T, Acc);
tokenize({{markup,tag},Buffer}, [H|T], Acc) when H==$\s; H==$\n; H==$\t; H==$\f; H==$\r ->
    tokenize({{markup,attrname},[]}, T, [atomize(Buffer)|Acc]);
tokenize({{markup,attrname},[]}, [$/|T], Acc) ->
    tokenize({{markup,tag},[]}, T, [slash|Acc]);
tokenize({{markup,attrname},Buffer}, [H|T], Acc) when H==$\s; H==$\n; H==$\t; H==$\f; H==$\r ->
    tokenize({markup,attrname}, T, [atomize(Buffer)|Acc]);
tokenize({markup,attrname}, [$=|T], Acc) ->
    tokenize({{markup,attrvalue},[]}, T, Acc);
tokenize({{markup,attrname},Buffer}, [$=|T], Acc) ->
    tokenize({{markup,attrvalue},[]}, T, [atomize(Buffer)|Acc]);
tokenize({{markup,attrvalue},[]}, [$"|T], Acc) ->
    tokenize({{markup,attrvalue},[]}, T,  Acc);
tokenize({{markup,attrvalue},Buffer}, [$"|T], Acc) ->
    tokenize({{markup,attrname},[]}, T, [lists:reverse(Buffer)|Acc]);
tokenize({{markup,_},[]}, [$>|T], Acc) ->
    tokenize({data,[]}, T, [gt|Acc]);
tokenize({{markup,_},Buffer}, [$>|T], Acc) ->
    tokenize({data,[]}, T, [gt|[atomize(Buffer)|Acc]]);
tokenize({{markup,tag},[]}, [$/|T], Acc) ->
    tokenize({{markup,tag}, []}, T, [slash|Acc]);
tokenize({{markup,tag},Buffer}, [$/|T], Acc) ->
    tokenize({{markup,tag},[]}, T, [slash,atomize(Buffer)|Acc]);
tokenize({{markup,Part},Buffer}, [Head|T], Acc) ->
    tokenize({{markup,Part},[Head|Buffer]}, T, Acc);
tokenize({data,Buffer}, [Head|T], Acc) ->
    tokenize({data,[Head|Buffer]}, T, Acc);
tokenize({data,[]}, [], Acc) ->
    lists:reverse(Acc).

atomize(List) ->
    list_to_atom(lists:reverse(List)).

markup(Tag,Attributes,[]) ->
    case is_always_empty_tag(Tag) of
	true ->
	    lists:flatten([$<,Tag,markup(Attributes),$ ,$/,$>]);
	false ->
	    markup(Tag,Attributes," ")
    end;
markup(Tag,Attributes,String) ->
    lists:flatten([$<,Tag,markup(Attributes),$>,String,$<,$/,Tag,$>]).

markup([{Name,Value}|T]) when atom(Name),list(Value) ->
    [$ ,atom_to_list(Name),$=,$",Value,$"|markup(T)];
markup([]) ->
    [].

parse_text([$*|T],Out,newline) ->
    parse_text(T,Out,{h,1,[]});
parse_text([$*|T],Out,{h,N,[]}) ->
    parse_text(T,Out,{h,N+1,[]});
parse_text([$ |T],Out,{h,N,[]}) ->
    parse_text(T,Out,{h,N,[]});
parse_text([$\n|T],Out,{h,N,Heading}) ->
    Tag = list_to_atom([$h|integer_to_list(N)]),
    parse_text(T,[{Tag,[],[lists:reverse(Heading)]}|Out],newline);
parse_text([$\n|T],Out,newline) ->
    parse_text(T,Out,newline);
parse_text([H|T],Out,{h,N,Heading}) ->
    parse_text(T,Out,{h,N,[H|Heading]});
parse_text([H|T],Out,newline) ->
    parse_text(T,Out,{p,[H]});
parse_text([$\n|[$\n|T]],Out,{p,Para}) ->
    parse_text(T,[{p,[],[lists:reverse(Para)]}|Out],newline);
parse_text([$\n|T],Out,{p,Para}) ->
    parse_text(T,Out,{p,[$ |Para]});
parse_text([H|T],Out,{p,Para}) ->
    parse_text(T,Out,{p,[H|Para]});
parse_text([],Out,{h,N,Heading}) ->
    Tag = list_to_atom([$h|integer_to_list(N)]),
    [{Tag,[],[lists:reverse(Heading)]}|Out];
parse_text([],Out,{p,Para}) ->
    [{p,[],[lists:reverse(Para)]}|Out];
parse_text([],Out,newline) ->
    Out.

is_always_empty_tag(Tag) when atom(Tag) ->
    lists:member(Tag, always_empty_tags());
is_always_empty_tag(Tag) when list(Tag) ->
    is_always_empty_tag(list_to_atom(Tag)).

usually_not_empty_tags() ->
    [a,html,head,link,title,p,h1,h2,body].

always_empty_tags() ->
    [meta,hr,br].

find_paths({html,_,_},html) ->
    [[html]];
find_paths({html,_,[H|T]},Tag) ->
    adlib:unique(lists:reverse(find_paths(T,Tag,[html],find_paths(H,Tag,[html],[]))));
find_paths({html,_,[]},_) ->
    [].

find_paths([H|_],_,_,Acc) when integer(H) ->
    Acc;
find_paths([H|T],Tag,Current,Acc) ->
    find_paths(T,Tag,Current,find_paths(H,Tag,Current,Acc));
find_paths({Tag,_,Content},Tag,Current,Acc) ->
    Path = lists:reverse([Tag|Current]),
    find_paths(Content,Tag,[Tag|Current],[Path|Acc]);
find_paths({Tag,_,Content},WantedTag,Current,Acc) ->
    find_paths(Content,WantedTag,[Tag|Current],Acc);
find_paths([],_Tag,_Current,Acc) ->
    Acc.

find_elements(Tree,Path) ->
    lists:flatten(find_elements2(Tree,Path)).

find_elements2(_Tree,[]) ->
    [];
find_elements2(Tree={Tag,_,_},[Tag]) ->
    [Tree];
find_elements2({Tag,_,Content},[Tag|T]) ->
    find_elements2(Content,T);
find_elements2({Tag,_,_},[Wanted|_]) when Tag /= Wanted ->
    [];
find_elements2([H|T],Path) ->
    [find_elements2(H,Path)|find_elements2(T,Path)];
find_elements2([],_Path) ->
    [].

