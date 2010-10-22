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

-module(erl_aliases).
-export([parse_transform/2]).


% "Abstract Format" chapter from ERTS User Guide
% http://www.erlang.org/doc/apps/erts/absform.html 
%
% Testing:
%       compile:file("t.erl", 'P').
%       compile:file("t.erl", 'E').


-define(LIST_MAPPER(M, F),
    M(L) -> lists:map(fun F/1, L)).


parse_transform(Forms, _Options) ->
    try
        lists:flatmap(fun rewrite/1, Forms)
    catch
        {error, Es} -> exit(Es);
        {error, Es, Line} ->
            File = get_file(),
            Error = {File,[{Line,compile,{parse_transform,?MODULE,Es}}]},
            {error, [Error], []};
        error ->
            io:format("non-trasformed expr: ~p~n", [erlang:get_stacktrace()])
    end.


set_file(File) ->
    put('__ra_file__', File).

get_file() ->
    get('__ra_file__').


add_alias(RecordName, Alias) ->
    put(Alias, RecordName).


unalias(X) ->
    case get(X) of
        'undefined' -> X;
        Name -> unalias(Name)
    end.


rewrite(X = {attribute,_LINE,file,{File,_Line}}) ->
    set_file(File), [X];

rewrite(X = {attribute,_LINE,record_alias,{Alias, RecordName}})
        when is_atom(RecordName), is_atom(Alias) ->
    io:format("record alias: ~w~n", [X]),
    add_alias(RecordName, Alias),
    [];

rewrite({attribute,LINE,record_alias,X}) ->
    %Es = lists:flatten(io_lib:format(
    %      "invalid 'record_alias' specification ~w at line ~w", [X, LINE])),
    %throw({error, Es});
    Es = ["invalid 'record_alias' specification", X],
    throw({error, Es, LINE});

rewrite({function,LINE,Name,Arity,L}) ->
    X = {function,LINE,Name,Arity, clause_list(L)},
    [X];

rewrite(X) -> [X].


?LIST_MAPPER(clause_list, clause).

clause({clause,LINE,Ps,Gs,B}) ->
    {clause,LINE, expr_list(Ps), expr_list_list(Gs), expr_list(B)}.


?LIST_MAPPER(expr_list, expr).
?LIST_MAPPER(expr_list_list, expr_list).

expr({record,LINE,Name,L}) ->
    {record,LINE,unalias(Name),field_list(L)};

expr({record,LINE,E,Name,L}) ->
    {record,LINE,E,unalias(Name),field_list(L)};

expr({record_index,LINE,Name,F}) ->
    {record_index,LINE,unalias(Name),F};

expr({record_field,LINE,E,Name,F}) ->
    {record_field,LINE,E,unalias(Name),F};

expr({match,LINE,P_1,P_2}) ->
    {match,LINE,expr(P_1),expr(P_2)};

expr({tuple,LINE,L}) ->
    {tuple,LINE,expr_list(L)};

expr({cons,LINE,P_h,P_t}) ->
    {cons,LINE,expr(P_h),expr(P_t)};

expr({bin,LINE, L}) ->
    {bin,LINE, bin_list(L)};

expr({op,LINE,Op,P_1,P_2}) ->
    {op,LINE,Op,expr(P_1),expr(P_2)};

expr({op,LINE,Op,P}) ->
    {op,LINE,Op,expr(P)};

expr({'catch',LINE,E}) ->
    {'catch',LINE,expr(E)};

expr({call, LINE, {remote,LINE_1,M,F}, L}) ->
    {call, LINE, {remote,LINE_1,expr(M),expr(F)}, expr_list(L)};

expr({call,LINE,F,L}) ->
    {call,LINE,expr(F),expr_list(L)};

% list comprehension
expr({lc,LINE,E,L}) ->
    expr({lc,LINE,expr(E),L});

% binary comprehension
expr({bc,LINE,E,L}) ->
    expr({bc,LINE,expr(E),L});

% begin .. end
expr({block,LINE,B}) ->
    {block,LINE,expr_list(B)};

expr({'if',LINE,L}) ->
    {'if',LINE,expr_list(L)};

expr({'case',LINE,E_0,L}) ->
    {'case',LINE,expr(E_0),clause_list(L)};

expr({'try',LINE,B,Clauses,CatchClauses,After}) ->
    {'try',LINE,expr(B),clause_list(Clauses),clause_list(CatchClauses),expr(After)};

expr({'receive',LINE,L,E_0,B_t}) ->
    {'receive',LINE,expr_list(L),expr(E_0),expr(B_t)};

% TODO:
% {'fun',LINE,{function,Name,Arity}}
% {'fun',LINE,{function,Module,Name,Arity}}
expr({'fun',LINE,{clauses,L}}) ->
    {'fun',LINE,{clauses,clause_list(L)}};

% If E is query [E_0 || W_1, ..., W_k] end, where each W_i is a generator or a filter
expr({'query',LINE,{lc,LINE,E_0,L}}) ->
    {'query',LINE,{lc,LINE,expr(E_0),gen_list(L)}};

% If E is E_0.Field, a Mnesia record access inside a query
expr({record_field,LINE,E_0,Field}) ->
    {record_field,LINE,expr(E_0),Field};

expr(Atomic = {A,_LINE,_L}) when A == 'integer'; A == 'float'; A == 'string'; A == 'atom' ->
    Atomic;

expr(Var = {var,_LINE,_A}) -> % variable
    Var;

expr(_Cc = {Class,P,_X}) when is_atom(Class) -> % throw or other atom catch clause
    io:format("catch clause: ~p~n", [_Cc]),
    {Class,expr(P),_X};

expr(X) ->
    io:format("non-trasformed expr: ~p~n", [X]),
    throw(error),
    X.


?LIST_MAPPER(field_list, field).

field({record_field,LINE,F,E}) ->
    {record_field,LINE,F,expr(E)}.


?LIST_MAPPER(bin_list, bin).

bin({bin_element,LINE,P,Size,TSL}) ->
    {bin_element,LINE,expr(P),Size,TSL}.


?LIST_MAPPER(gen_list, gen).

gen({generate,LINE,P,E}) ->
    {generate,LINE,expr(P),expr(E)};

gen({b_generate,LINE,P,E}) ->
    {b_generate,LINE,expr(P),expr(E)};

gen(X) -> expr(X).

