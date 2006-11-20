%%% Copyright (c) 2004-2005 Dominic Williams, Nicolas Charpentier,
%%% Fabrice Nourisson, Jacques Couvreur, Virgile Delecolle.
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

-module(testing_ut).
-compile(export_all).

%% Actual test functions: use _test suffix

runner_test() ->
    Self = self(),
    Notify = fun(Token, Message) -> Self ! {Token, Message} end,
    Runner = spawn(testing, runner, [Notify]),
    lists:foldl(
      fun({Fun, Predicate}, Count) ->
	      Token = make_ref (),
	      Runner ! {Token, test, Fun},
	      {Token, Result} = testing:receive_one(),
	      {Count, true} = {Count, Predicate(Result)},
	      Count + 1
      end,
      1,
      [{fun successful_test_/0, fun(Result) -> Result == pass end},
       {fun unsuccessful_test_/0, fun({fail,_}) -> true; (_) -> false end}]),
    true = is_process_alive (Runner),
    Runner ! stop.

run_functions_test() ->
    {1,[]} = testing:run_functions([fun successful_test_/0]),
    {2,[_Reason]} = testing:run_functions([fun successful_test_/0, fun unsuccessful_test_/0]),
    pass.

run_modules_test() ->
    [{?MODULE,2,[_Error]}] = 
		testing:run_modules([?MODULE], adlib:ends_with("_withfunnysuffix")),
    [{?MODULE,3,[_OtherError]}] = 
		testing:run_modules([?MODULE], adlib:begins_with("funnyprefixed_")),
    pass.

%% Dummy test functions (used to test the real functions)

successful_test_() ->
    ok = ok.

unsuccessful_test_() ->
    exit (suicide).

successful_test_withfunnysuffix() ->
    successful_test_().

unsuccessful_test_withfunnysuffix() ->
    unsuccessful_test_().

funnyprefixed_successful_test_() ->
    successful_test_().

funnyprefixed_unsuccessful_test_() ->
    unsuccessful_test_().

funnyprefixed_another_successful_test_() ->
    successful_test_().

receive_one_from_test() ->
    MessageSender = fun (Pid, Message) ->
			    fun() ->  Pid ! {self(), Message} end
		    end,
    Pid1 = spawn(MessageSender(self(), hello)),
    Pid2 = spawn(MessageSender(self(), goodbye)),
    goodbye = testing:receive_one_from(Pid2),
    hello = testing:receive_one_from(Pid1),
    timeout = testing:receive_one_from(self()).

fake_file_system_test () ->    
    Tree = [{"/tmp", directory, {{2006,3,2},{12,56,00}},
	     [{"foo", regular, {{2006,3,18},{14,00,00}}},
	      {"bar", regular, {{2005,3,18},{9,00,03}}}]},
	    {"/home/dodo/.emacs", regular, {{2006,1,15},{20,44,00}}}],
    F = testing:file_system (Tree),
    F ! {self(), "/tmp", [directory_content]},		   
    F ! {self(), "/tmp/foo", [type]},			   
    F ! {self(), "/tmp/bar", [modification_time]},	   
    F ! {self(), "/home/dodo/.emacs", [directory_content]},
    Messages = testing:receive_all(),
    same_elements =
	adlib:compare (
	  [{F, "/tmp", [{directory_content, ["foo", "bar"]}]},
	   {F, "/tmp/foo", [{type, regular}]},	   
	   {F, "/tmp/bar", [{modification_time, {{2005,3,18},{9,00,03}}}]},	   
	   {F, "/home/dodo/.emacs", [{directory_content, {error, enotdir}}]}],
	  Messages),
    F ! stop.
