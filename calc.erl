-module(calc).
-export([rpn/1]).
%%===========================
%% not ready
%%===========================

rpn(L) when is_list(L) ->
    Expr = string:tokens(L, " "),
    [Res] = lists:foldl(fun parse/2, [], Expr),
    Res.

parse("+", [X,Y|Rest]) ->
    [X+Y|Rest];
parse("-", [X,Y|Rest]) ->
    [X-Y|Rest];
parse("*", [X,Y|Rest]) ->
    [X*Y|Rest];
parse("/", [X,Y|Rest]) ->
    [X/Y, Rest];
parse("^", [X,Y|Rest]) ->
    [math:pow(X,Y)|Rest];
parse("rem", [X,Y|Rest]) ->
    [X rem Y|Rest];
parse("div", [X,Y|Rest]) ->
    [X div Y|Rest];
parse("sqrt", [X|Rest]) ->
    [math:sqrt(X)|Rest];
parse(Num, Rest) ->
    [read(Num)|Rest].

read(N) -> 
    case string:to_float(N) of
	{error, no_float} ->
	    list_to_integer(N);
	{F,_} ->
	    F
    end.

parse_unit(Value) ->
    case Value of
        "!" -> fun neg/1;
        "ln" -> fun ln/1;
        "log10" -> fun log10/1;
        "log2" -> fun log2/1;
        "sqrt" -> fun sqrt/1;
        "+" -> fun plus/1;
        "*" -> fun prod/1;
        _ -> notimplemented
    end.

% unary operators
neg(X) -> not X.
ln(X) -> math:log(X).
log10(X) -> math:log10(X).
log2(X) -> math:log2(X).
sqrt(X) -> math:sqrt(X).

% binary operators
power(X, Y) -> math:pow(X, Y).
float_division(X, Y) -> X/Y.
int_division(X, Y) -> X div Y.
remainder(X, Y) -> X rem Y.

eq(X, Y) -> X==Y.
bigger(X, Y) -> X>Y.
lesser(X, Y) -> X<Y.
beq(X, Y) -> X>=Y.
leq(X, Y) -> X=<Y.

% arbitrary arity assoc operators
plus(List) -> lists:foldl(fun(X,Y) -> X+Y end, 0, List).
prod(List) -> lists:foldl(fun(X,Y) -> X*Y end, 1, List).
% arbitrary arity non-assoc operators

un_op(Val) ->
    fun(X) ->
        case Val of
            "!" -> not X;
            "ln" -> math:log(X);
            "log10" -> math:log10(X);
            "log2" -> math:log2(X);
            "sqrt" -> math:sqrt(X)
        end
    end.

bin_op(Val) ->
    fun(X, Y) ->
        case Val of
            "+" -> X+Y;%
            "-" -> X-Y;
            "*" -> X*Y;%
            "/" -> X/Y;%
            "rem" -> X rem Y;%
            "div" -> X div Y;%
            "||" -> X or Y;
            "&&" -> X and Y;
            "xor" -> X xor Y;
            "^" -> math:pow(X, Y);%
            "=" -> X==Y;%
            ">" -> X>Y;%
            "<" -> X<Y;%
            ">=" -> X >= Y;%
            "<=" -> X =< Y%
        end
    end.
