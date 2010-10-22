%%  Copyright (c) 2010 Anton Lavrik, http://github.com/alavrik
%%  
%%  Permission is hereby granted, free of charge, to any person obtaining
%%  a copy of this software and associated documentation files (the
%%  "Software"), to deal in the Software without restriction, including
%%  without limitation the rights to use, copy, modify, merge, publish,
%%  distribute, sublicense, and/or sell copies of the Software, and to
%%  permit persons to whom the Software is furnished to do so, subject to
%%  the following conditions:
%%  
%%  The above copyright notice and this permission notice shall be
%%  included in all copies or substantial portions of the Software.
%%  
%%  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
%%  EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
%%  MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
%%  NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
%%  LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
%%  OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
%%  WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

-module(erl_aliases_tests).

%-compile(export_all).
-include_lib("erl_aliases.hrl").
-include_lib("eunit/include/eunit.hrl").


-record(long_record_name, {a, b}).

-record_alias({r, long_record_name}).
%-module_alias({m, long_module_name}).


r_match(#r{a = 'undefined', b = 'undefined'}) -> ok;
r_match(R = #r{a = A}) when R#r.a == a, A == b -> ok;
r_match(#r{}) -> ok.


r_test() ->
    R = #r{},
    R#r.a,
    _ = #r.b,
    #r{a = _A, b = _B} = R,
    r_match(R),

    R1 = R#r{a = 1, b = 2},
    r_match(R1),

    case R1 of
        #r{a = 1, b = B} when B == c -> ok;
        #r{} -> ok;
        X when X#r.a == 1 -> ok
    end,

    T = {R#r.a, R1#r.b},

    F = fun
        (R2 = #r{}) when R2#r.a /= #r.a -> ok;
        (#r{a = 1}) -> ok;
        (_) -> T
    end,

    F(R),

    ok.

