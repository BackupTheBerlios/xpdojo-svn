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

-module(xhtml_ut).
-compile(export_all).
-import(xhtml,[find_elements/2,find_paths/2,doctype/0,generate/1,parse/1,tokenize/1,from_text/1,escape/1]).

escape_plain_test() ->
    "Hello world" = escape("Hello world").

escape_ltgt_test() ->
    "2 &lt; 3" = escape("2 < 3"),
    "4 &gt;= X" = escape("4 >= X").

escape_amp_test() ->
    "a &amp; b" = escape("a & b").

example_text() ->
    "* Heading 1\n\n"
	"This is a paragraph\n"
	"despite the newline.\n\n"
	"An empty line creates a\n"
	"new paragraph.".

from_text_simple_test() ->
    Expected = [{h1,[],["Heading 1"]},
		{p,[],["This is a paragraph despite the newline."]},
		{p,[],["An empty line creates a new paragraph."]}],
    Expected = from_text(example_text()).

from_text_heading_levels_test() ->
    [{h1,[],["Heading 1"]},{p,[],["Intro"]},{h2,[],["Heading 2"]},{p,[],["Blurb."]}] =
	from_text("* Heading 1\n\n"
		  "Intro\n\n"
		  "** Heading 2\n\n"
		  "Blurb.").

tokenize_doctype_test() ->
    [doctype] = tokenize(xhtml:doctype()).

tokenize_empty_test() ->
    [doctype,lt,html,gt,lt,slash,html,gt] = tokenize(xhtml:doctype()++"<html></html>").

tokenize_hello_test() ->
    [doctype,lt,html,gt,"Hello",lt,slash,html,gt] = tokenize(xhtml:doctype()++"<html>Hello</html>").

tokenize_hello_nodoctype_test() ->
    [lt,html,gt,"Hello",lt,slash,html,gt] = tokenize("<html>Hello</html>").

tokenize_complex_test() ->
    Input = xhtml:doctype() ++ "<html><body><p>Hello world!</p></body></html>",
    ExpectedTokens = [doctype,lt,html,gt,lt,body,gt,lt,p,gt,"Hello world!",
		      lt,slash,p,gt,lt,slash,body,gt,lt,slash,html,gt],
    ExpectedTokens = tokenize(Input).

tokenize_attributes_test() ->
    Input = xhtml:doctype() ++ "<html lang=\"fr\"><body><p class=\"big\">Hello</p></body></html>",
    ExpectedTokens = [doctype,lt,html,lang,"fr",gt,lt,body,gt,lt,p,class,"big",gt,"Hello",
		      lt,slash,p,gt,lt,slash,body,gt,lt,slash,html,gt],
    ExpectedTokens = tokenize(Input).

tokenize_whitespace_test() ->
    Input = xhtml:doctype() ++ "  <\thtml\nlang =\r\f\"fr\"> < body >\n\t\t<p class=\t\"big\">Hello</p>\n\n\n</body>\n< / html>",
    ExpectedTokens = [doctype,lt,html,lang,"fr",gt,lt,body,gt,lt,p,class,"big",gt,"Hello",
		      lt,slash,p,gt,lt,slash,body,gt,lt,slash,html,gt],
    ExpectedTokens = tokenize(Input).

tokenize_empty_elements_test() ->
    Input = "<html/>",
    Expected_tokens = [lt,html,slash,gt],
    Expected_tokens = tokenize(Input),
    Input2 = "<html />",
    Expected_tokens = tokenize(Input2).

%%TODO: tokenize entities...?

doctype_test() ->
    "<!DOCTYPE html\n"
	"PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\"\n"
	"\"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">\n" = xhtml:doctype().

has_doctype_test() ->
    Doc = "<html>Hello</html>",
    true = xhtml:has_doctype(xhtml:doctype()++Doc),
    false = xhtml:has_doctype(Doc).

parse_simple_test() ->
    {html,[],[{body,[],["Hello"]}]} = parse("<html><body>Hello</body></html>").

parse_title_test() ->
    {html,[],[{head,[],[{title,[],["My title"]}]}]} = parse("<html><head><title>My title</title></head></html>").

parse_attribute_test() ->    
    {html,[{lang,"fr"}],[{body,[],[{p,[{class,"funky style"}],["G'day, mate"]}]}]} =
	parse(doctype()++"<html lang=\"fr\"><body><p class=\"funky style\">G'day, mate</p></body></html>").

parse_multiple_attribute_test() ->
    {html,[{lang,"fr"},{'xml:lang',"fr"}],[{head,[],[{meta,[{name,"author"},{content,"Dominic Williams"}],[]}]}]} =
	parse("<html lang=\"fr\" xml:lang=\"fr\"><head><meta name=\"author\" content=\"Dominic Williams\" /></head></html>").

parse_empty_elements_test() ->
    {html,[],[{body,[],[{p,[],["Hello",{hr,[],[]},"Goodbye",{hr,[],[]},"Bugger off",{hr,[],[]}]}]}]} =
	parse("<html><body><p>Hello<hr/>Goodbye<hr />Bugger off<hr></hr></p></body></html>").

title_test() ->
    Tree = parse("<html><head><title>My title</title></head></html>"),
    "My title" = xhtml:title(Tree).

generate_simple_test() ->
    Expected = doctype()++"<html>Hello</html>",
    Expected = generate({html,[],["Hello"]}).

generate_compound_test() ->
    Expected = doctype()++"<html><head><title>My title</title></head><body><p>Hello world</p></body></html>",
    Expected = generate({html,[],[{head,[],[{title,[],["My title"]}]},
				  {body,[],[{p,[],["Hello world"]}]}]}).

generate_attributes_test() ->
    Expected = doctype()++"<html lang=\"fr\"><head><meta name=\"author\" content=\"Me\" /><title>My title</title></head><body><p>Hello world</p></body></html>",
    Expected = generate({html,[{lang,"fr"}],[{head,[],[{meta,[{name,"author"},{content,"Me"}],[]},{title,[],["My title"]}]},
					     {body,[],[{p,[],["Hello world"]}]}]}).

generate_always_empty_for_old_browsers_test() ->
    Expected = doctype()++"<html><head><meta name=\"author\" content=\"Me\" /></head><body>Hello<br />you<hr /></body></html>",
    Expected = generate({html,[],[{head,[],[{meta,[{name,"author"},{content,"Me"}],[]}]},{body,[],["Hello",{br,[],[]},"you",{hr,[],[]}]}]}).

generate_unusually_empty_for_old_browsers_test() ->
    Expected = doctype()++"<html> </html>",
    Expected = generate({html,[],[]}).

attribute_simple_test() ->
    Tree = parse("<html lang=\"fr\"></html>"),
    ["fr"] = xhtml:attribute(Tree,html,lang).

attribute_despite_inner_element_test() ->
    Tree = parse("<html lang=\"fr\"><p>Hello</p></html>"),
    ["fr"] = xhtml:attribute(Tree,html,lang).

attribute_despite_other_tag_test() ->
    Tree = parse("<html lang=\"fr\"><p lang=\"en\">Hello</p></html>"),
    ["fr"] = xhtml:attribute(Tree,html,lang).

attribute_absent_test() ->
    Tree = parse("<html xml:lang=\"fr\"><p>Hello</p></html>"),
    [] = xhtml:attribute(Tree,html,lang).

attribute_inner_test() ->
    Tree = parse("<html lang=\"fr\"><p class=\"weird\">Hello</p></html>"),
    ["weird"] = xhtml:attribute(Tree,p,class).

attribute_among_others_test() ->
    Tree = parse("<html lang=\"fr\" xml:lang=\"en\"><p class=\"weird\">Hello</p></html>"),
    ["en"] = xhtml:attribute(Tree,html,'xml:lang').
    
%% attribute_of_nested_element_test() ->
%%     Tree = parse("<html lang=\"fr\"><p class=\"weird\">Hello</p></html>"),
%%     "weird" = xhtml:attribute(Tree,[html,p],class).

find_elements_empty_path_test() ->
    [] = find_elements({html,[],[]}, []).

find_elements_html_test() ->
    [{html,[],[]}] = find_elements({html,[],[]}, [html]).

find_elements_empty_tree_test() ->
    [] = find_elements({html,[],[]}, [p]).

find_elements_no_parent_test() ->
    [] = find_elements({html,[],[{body,[],[]}]}, [body]).

find_elements_one_level_test() ->
    [{body,[],[]}] = find_elements({html,[],[{body,[],[]}]}, [html,body]).

find_elements_three_levels_test() ->
    [{p,[],["Hello"]}] = find_elements({html,[],[{body,[],[{p,[],["Hello"]}]}]}, [html,body,p]).

find_elements_several_test() ->
    [{p,[],["Hello"]},{p,[],["Paragraph"]}] = find_elements({html,[],[{body,[],[{p,[],["Hello"]},{h1,[],["Title"]},{p,[],["Paragraph"]}]}]}, [html,body,p]).

find_elements_three_level_with_a_negative_test() ->
    [{title,[],["Title"]}] = find_elements({html,[],
					    [{head,[],[{title,[],["Title"]}]},
					     {body,[],[{h1,[],["Title"]},{p,[],["Para"]}]}]},
					   [html,head,title]).
    
find_paths_none_test() ->
    [] = find_paths({html,[],[]}, p),
    [] = find_paths({html,[],[{body,[],[{h1,[],["Title"]}]}]}, a).

find_paths_html_test() ->
    [[html]] = find_paths({html,[],[]}, html).

find_paths_one_level_test() ->
    [[html,body]] = find_paths({html,[],[{body,[],[{p,[],["Hello"]}]}]}, body).

find_paths_three_level_test() ->
    [[html,body,p,i]] = find_paths({html,[],[{body,[],[{p,[],["Hello",{i,[],["World"]}]}]}]}, i).

find_paths_several_test() ->
    [[html,body,b],[html,body,p,b]] = find_paths({html,[],[{body,[],[{b,[],["Hello"]},{p,[],["Hello ",{b,[],["again"]}]}]}]}, b).

find_paths_unique_results_test() ->
    [[html,body,p],[html,body,p,ol,li,p]] = find_paths({html,[],[{body,[],[{p,[],["One"]},{h1,[],["title"]},{p,[],["A list",{ol,[],[{li,[],[{p,[],["item"]}]}]}]},{p,[],["Two"]}]}]}, p).

language_valid_test() ->
    "jp" = xhtml:language(xhtml:parse("<html lang=\"jp\" xml:lang=\"jp\"><body><p>Hello</p></body></html>")).

language_incoherent_test() ->
    error = xhtml:language(xhtml:parse("<html lang=\"jp\" xml:lang=\"fr\"><body><p>Hello</p></body></html>")).
    
language_incomplete_test() ->
    error = xhtml:language(xhtml:parse("<html lang=\"en\"></html>")),
    error = xhtml:language(xhtml:parse("<html xml:lang=\"fr\"></html>")).

language_missing_test() ->
    error = xhtml:language(xhtml:parse("<html><body><p>Hello</p></body></html>")).

stylesheet_test() ->
    "dw.css" = xhtml:stylesheet(xhtml:parse("<html><head><link rel=\"stylesheet\" type=\"text/css\" href=\"dw.css\" /></head></html>")).

content_type_test() ->
    "text/html; charset=utf-8" = xhtml:content_type(xhtml:parse("<html><head><meta http-equiv=\"content-type\" content=\"text/html; charset=utf-8\" /><meta name=\"description\" content=\"rubbish\" /></head><body>Hello</body></html>")).

    

