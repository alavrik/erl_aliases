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


% uncomment this like to print some debug information
%-define(DEBUG,1).

-ifdef(DEBUG).
-compile(export_all).
-define(PRINT(Fmt, Args), io:format(Fmt, Args)).
-else.
-define(PRINT(Fmt, Args), ok).
-endif.


-define(LIST_MAPPER(M, F),
    M(L) -> lists:map(fun F/1, L)).


parse_transform(Forms, _Options) ->
    try
        lists:flatmap(fun rewrite/1, Forms)
    catch
        %{error, Es} -> exit(Es);
        {error, Es, Line} ->
            File = get_file(),
            Error = {File,[{Line,compile,{parse_transform,?MODULE,Es}}]},
            {error, [Error], []};
        {missing_rule, Term} ->
            Es = lists:flatten(io_lib:format(
                "missing transformation rule for: ~w, "
                "stack ~w", [Term, erlang:get_stacktrace()])),
            exit(Es)
    end.


set_file(File) ->
    put('__erl_aliases_file__', File).

get_file() ->
    get('__erl_aliases_file__').


add_record_alias(Name, Alias, Line) ->
    add_alias('__erl_aliases_records__', Name, Alias, Line).

unalias_record(Name) ->
    unalias('__erl_aliases_records__', Name).


add_module_alias(Name, Alias, Line) ->
    add_alias('__erl_aliases_modules__', Name, Alias, Line).

unalias_module(Name) ->
    unalias('__erl_aliases_modules__', Name).


unalias_module_atom({'atom', LINE, Name}) ->
    Name1 = unalias_module(Name),
    {'atom', LINE, Name1};
unalias_module_atom(X) -> X.


add_alias(DictName, Name, Alias, Line) ->
    ?PRINT("add ~w: ~w for ~w~n", [DictName, Alias, Name]),
    Dict =
        case get(DictName) of
            'undefined' ->
                % there are no prior dictionary and aliases; create an empty one
                dict:new();
            X -> X
        end,
    case dict:is_key(Alias, Dict) of
        true ->
            Es = lists:concat(["duplicate alias: ", Alias]),
            throw({error, Es, Line});
        false ->
            NewDict = dict:store(Alias, Name, Dict),
            put(DictName, NewDict),
            ok
    end.


unalias(DictName, X) ->
    case get(DictName) of
        'undefined' -> X;  % there is no dictionary and aliases
        Dict -> unalias_1(DictName, Dict, X)
    end.


unalias_1(_DictName, Dict, X) ->
    case dict:find(X, Dict) of
        'error' -> X; % no alias; return the original name
        {ok, Name} ->
            ?PRINT("unalias ~w: ~w to ~w~n", [_DictName, X, Name]),
            % resolved name can also be an alias
            unalias_1(_DictName, Dict, Name)
    end.


rewrite(X = {attribute,_LINE,file,{File,_Line}}) ->
    set_file(File), [X];

rewrite({attribute,LINE,record_alias,{Alias, RecordName}})
        when is_atom(RecordName), is_atom(Alias) ->
    add_record_alias(RecordName, Alias, LINE),
    [];

rewrite({attribute,LINE,record_alias,X}) ->
    %Es = lists:flatten(io_lib:format(
    %      "invalid 'record_alias' specification ~w at line ~w", [X, LINE])),
    %throw({error, Es});
    Es = ["invalid 'record_alias' specification", X],
    throw({error, Es, LINE});

rewrite({attribute,LINE,module_alias,{Alias, ModuleName}})
        when is_atom(ModuleName), is_atom(Alias) ->
    add_module_alias(ModuleName, Alias, LINE),
    [];

rewrite({attribute,LINE,module_alias,X}) ->
    Es = ["invalid 'module_alias' specification", X],
    throw({error, Es, LINE});

rewrite({function,LINE,Name,Arity,L}) ->
    X = {function,LINE,Name,Arity, clause_list(L)},
    [X];

rewrite(X) -> [X].


?LIST_MAPPER(clause_list, clause).

clause(_C = {clause,LINE,Ps,Gs,B}) ->
    %?PRINT("clause: ~p~n", [_C]),
    {clause,LINE, expr_list(Ps), expr_list_list(Gs), body(B)}.


?LIST_MAPPER(expr_list, expr).
?LIST_MAPPER(expr_list_list, expr_list).


body(X) -> expr_list(X).


expr({record,LINE,Name,L}) ->
    {record,LINE,unalias_record(Name),field_list(L)};

expr({record,LINE,E,Name,L}) ->
    {record,LINE,E,unalias_record(Name),field_list(L)};

expr({record_index,LINE,Name,F}) ->
    {record_index,LINE,unalias_record(Name),F};

expr({record_field,LINE,E,Name,F}) ->
    {record_field,LINE,E,unalias_record(Name),F};

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
    M1 = unalias_module_atom(M),
    {call, LINE, {remote,LINE_1,expr(M1),expr(F)}, expr_list(L)};

expr({call,LINE,F,L}) ->
    {call,LINE,expr(F),expr_list(L)};

% list comprehension
expr({lc,LINE,E,L}) ->
    {lc,LINE,expr(E),gen_list(L)};

% binary comprehension
expr({bc,LINE,E,L}) ->
    {bc,LINE,expr(E),gen_list(L)};

% If E is query [E_0 || W_1, ..., W_k] end, where each W_i is a generator or a filter
expr({'query',LINE,{lc,LINE,E_0,L}}) ->
    {'query',LINE,{lc,LINE,expr(E_0),gen_list(L)}};

% begin .. end
expr({block,LINE,B}) ->
    {block,LINE,body(B)};

expr({'if',LINE,L}) ->
    {'if',LINE,clause_list(L)};

expr({'case',LINE,E_0,L}) ->
    {'case',LINE,expr(E_0),clause_list(L)};

% try ... of ... catch ... after ... end
expr(_X = {'try',LINE,B,Clauses,CatchClauses,After}) ->
    %?PRINT("try: ~p~n", [_X]),
    {'try',LINE,body(B),clause_list(Clauses),clause_list(CatchClauses),body(After)};

% receive ... end
expr({'receive',LINE,L,E_0,B_t}) ->
    {'receive',LINE,clause_list(L),expr(E_0),body(B_t)};

% receive ... after ... end
expr({'receive',LINE,L}) ->
    {'receive',LINE,clause_list(L)};

expr(X = {'fun',_LINE,{function,_Name,_Arity}}) ->
    X;
expr({'fun',LINE,{function,Module,Name,Arity}}) ->
    {'fun',LINE,{function,unalias_module(Module),Name,Arity}};

expr({'fun',LINE,{clauses,L}}) ->
    {'fun',LINE,{clauses,clause_list(L)}};

% If E is E_0.Field, a Mnesia record access inside a query
expr({record_field,LINE,E_0,Field}) ->
    {record_field,LINE,expr(E_0),Field};

expr(Atomic = {A,_LINE,_Value})
        when A == 'integer'; A == 'float'; A == 'string';
             A == 'atom'; A == 'char' ->
    Atomic;

expr(Var = {var,_LINE,_A}) -> % variable or variable pattern
    Var;

expr(X = {nil,_LINE}) -> X;

% If C is a catch clause X : P when Gs -> B where X is an atomic literal or a
% variable pattern, P is a pattern, Gs is a guard sequence and B is a body, then
% Rep(C) = {clause,LINE,[Rep({X,P,_})],Rep(Gs),Rep(B)}.
expr(_Cc = {Class,P,'_'}) ->
    %?PRINT("catch clause: ~p~n", [_Cc]),
    {expr(Class),expr(P),'_'};

expr(X) ->
    throw({missing_rule, X}).
    %X.


?LIST_MAPPER(field_list, field).

field({record_field,LINE,F,E}) ->
    {record_field,LINE,F,expr(E)}.


?LIST_MAPPER(bin_list, bin).

bin({bin_element,LINE,P,Size,TSL}) ->
    {bin_element,LINE,expr(P),Size,TSL}.


% When W is a generator or a filter (in the body of a list or binary comprehension), then:
%
%    * If W is a generator P <- E, where P is a pattern and E is an expression, then Rep(W) = {generate,LINE,Rep(P),Rep(E)}.
%    * If W is a generator P <= E, where P is a pattern and E is an expression, then Rep(W) = {b_generate,LINE,Rep(P),Rep(E)}.
%    * If W is a filter E, which is an expression, then Rep(W) = Rep(E).

?LIST_MAPPER(gen_list, gen).

gen({generate,LINE,P,E}) ->
    {generate,LINE,expr(P),expr(E)};

gen({b_generate,LINE,P,E}) ->
    {b_generate,LINE,expr(P),expr(E)};

gen(X) -> expr(X).

