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

-module_alias({m, erl_aliases_tests}).


-export([m_test_helper/0]).


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

    if
        R#r.a == 'undefined'; not R1#r.b == 'undefined' -> ok
    end,

    begin
        F = fun
            (R2 = #r{}) when R2#r.a /= #r.a -> ok;
            (#r{a = 1}) -> ok;
            (_) -> T
        end,

        F(R)
    end,

    try foo, ok of
        ok -> ok1;
        ok when ok == ok -> ok2
    catch
        x -> ok;
        {e, E} when E#r.b /= 0 -> -E#r.a;
        throw:{e, E1} -> E1#r{b = 0};
        E -> E#r.a;
        T:x -> ok
    after
        R#r.b
    end,

    catch R#r.a,

    self() ! [],
    receive
        [] -> ok;
        _ when R#r.b /= 1 -> R#r.a
    end,

    receive
        [] -> ok;
        _ when R#r.b /= 1 -> R#r.a
    after
        R1#r.a -> R#r.b
    end,

    [ X#r.a || X <- [R, R1], is_integer(X#r.a) ],

    <<3:32>> = <<(R1#r.a + R1#r.b):32/integer>>,

    % TODO: missing tests:
    % binary comprehension
    % query

    ok.


m_test() ->
    m:m_test_helper(),
    F = fun m:m_test_helper/0,
    F(),
    ok.


m_test_helper() -> ok.

