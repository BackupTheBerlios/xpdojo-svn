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

-module(testing_ut).
-compile(export_all).

%% Actual test functions: use _test suffix

run_functions_test() ->
    {1,[]} = testing:run_functions([fun successful_test_/0]),
    {2,[_Reason]} = testing:run_functions([fun successful_test_/0, fun unsuccessful_test_/0]),
    pass.

run_modules_test() ->
    {2,[_Reason1]} = testing:run_modules([?MODULE], adlib:ends_with("_withfunnysuffix")),
    {3,[_Reason2]} = testing:run_modules([?MODULE], adlib:begins_with("funnyprefixed_")),
    pass.

%% Dummy test functions (used to test the real functions)

successful_test_() ->
    ok = ok.

unsuccessful_test_() ->
    ok = nok.

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

