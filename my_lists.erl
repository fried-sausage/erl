-module(my_lists).
-export([
        all/2, any/2
        , append/1
        , delete/2
        , last/1, droplast/1
        , duplicate/2
        , flatlength/1
        , map/2, filter/2
        , flatmap/2, filtermap/2
        , partition/2, takewhile/2, dropwhile/2
        , foldl/3, foldr/3, sum/1
        , mapfoldl/3, mapfoldr/3
        , flatten/1, flatten/2
        , foreach/2
        , length/1
        , reverse/1
        , max_/1, min_/1
        , member/2
        , head/1, nth/2, nthtail/2
        , prefix/2, suffix/2
        , seq/1, seq/2, seq/3
        , split/2, splitwith/2
        , sublist/2, sublist/3
        , subtract/2
        , zip/2, zip3/3, zipwith/3, zipwith3/4
        , stub/0
        ]).

%reimplement stdlib's lists module
stub() -> ok.

all(_, []) -> true;
all(Pred, [H|T]) ->
    case Pred(H) of
        false -> false;
        true -> all(Pred, T)
    end.

any(_, []) -> false;
any(Pred, [H|T]) ->
    case Pred(H) of
        true -> true;
        false -> any(Pred, T)
    end.

append([]) -> [];
append([H|T]) -> H ++ append(T).

delete(Elem, Lst) -> delete([], Elem, Lst).

delete(Acc, _, []) -> reverse(Acc);
delete(Acc, Elem, [H|T]) ->
    case (Elem =:= H) of
        false -> delete([H|Acc], Elem, T);
        true -> reverse(Acc) ++ T
    end.


last([Value|[]]) -> Value;
last([_|T]) -> last(T).

droplast(Lst) -> droplast([], Lst).
droplast(Acc, [_|[]]) -> reverse(Acc);
droplast(Acc, [H|T]) -> droplast([H|Acc], T).

dropwhile(Pred, []) when is_function(Pred, 1) -> [];
dropwhile(Pred, [H|T]) -> 
    case Pred(H) of
        false -> [H|T];
        true -> dropwhile(Pred, T)
    end.

takewhile(Pred, List) when is_function(Pred, 1) ->
    takewhile(Pred, List, []).

takewhile(_, [], Acc) ->
    reverse(Acc);
takewhile(Pred, [H|T], Acc) ->
    case Pred(H) of
        true ->  takewhile(Pred, T, [H|Acc]);
        false -> reverse(Acc)
    end.

duplicate(0, _) -> [];
duplicate(N, Elem) when N>0 -> [Elem|duplicate(N-1, Elem)].

filtermap(_, []) -> [];
filtermap(F, Lst) -> foldr(
                            fun(Acc, Head) ->
                                case F(Head) of
                                    false -> Acc;
                                    true -> [Head|Acc];
                                    {true, Value} -> [Value|Acc]
                                end
                            end
                            , []
                            , Lst
                            ).

flatlength(L) when is_list(L) -> do_flen(L).
do_flen([]) -> 0;
do_flen([H|T]) -> flatlength(H) + flatlength(T);
do_flen(_) -> 1.

flatmap(Fun, List1) -> append(map(Fun, List1)).

flatten(L) when is_list(L) -> do_flat(L, []).
flatten(L, Tail) when is_list(L), is_list(Tail) -> do_flat(L, Tail).

do_flat([H|T], Tail) when is_list(H) ->
    do_flat(H, do_flat(T, Tail));
do_flat([Value|T], Tail) ->
    [Value|do_flat(T, Tail)];
do_flat([], Tail) ->
    Tail.

foldl(F, Acc, [H|T]) ->
    foldl(F, F(H, Acc), T);
foldl(F, Acc, []) when is_function(F, 2) ->
    Acc.

foldr(F, Acc, Lst) -> foldl(F, Acc, reverse(Lst)).
%> (foldr - 0 '(1 2 3 4))
%> (- 1 (- 2 (- 3 (- 4 0))))
%-2
%> (foldl - 0 '(1 2 3 4))
%> (- 4 (- 3 (- 2 (- 1 0))))
%2
foreach(Fun, []) when is_function(Fun, 1) ->
    ok;
foreach(Fun, [H|T]) ->
    Fun(H),
    foreach(Fun, T).

map(Fun, Lst) -> map([], Fun, Lst).
map(Acc, Fun, []) when is_function(Fun, 1) -> reverse(Acc);
map(Acc, Fun, [H|T]) -> map([Fun(H)|Acc], Fun, T).

filter(Pred, Lst) when is_function(Pred, 1) -> filter(Pred, Lst, []).
filter(_, [], Acc) -> reverse(Acc);
filter(Pred, [H|T], Acc) ->
    case Pred(H) of
        true ->  filter(Pred, T, [H|Acc]);
        false -> filter(Pred, T, Acc)
    end.

length(L) -> len(0, L).
len(Acc, []) -> Acc;
len(Acc, [_|T]) -> len(Acc+1, T).

reverse(Lst) -> rev([], Lst).
rev(Acc, []) -> Acc;
rev(Acc, [H|T]) -> rev([H|Acc], T).

%mapfoldl(Fun, Acc0, List1) -> {List2, Acc1}.
mapfoldl(F, Acc, Lst) when is_function(F, 2) ->
    do_mapfoldl(F, Acc, Lst).

do_mapfoldl(_, Acc, []) ->
    Acc;
do_mapfoldl(F, {Item, _}, [H|T]) ->
    {NewItem, NewAcc} = F(H),
    do_mapfoldl(F, {[NewItem|Item], NewAcc}, T).

mapfoldr(F, Acc, Lst) ->
    {NewLst, Res} = mapfoldl(F, Acc, Lst),
    {reverse(NewLst), Res}.

max_([H|T]) -> max_(H, T).
max_(Acc, []) -> Acc;
max_(Acc, [H|T]) ->
    case (Acc >= H) of
        true  -> max_(Acc, T);
        false -> max_(H, T)
    end.

min_([H|T]) -> min_(H, T).
min_(Acc, []) -> Acc;
min_(Acc, [H|T]) ->
    case (Acc >= H) of
        true  -> min_(Acc, T);
        false -> min_(H, T)
    end.

member(El, [El|_]) -> true;
member(El, [_|T]) when is_list(T) -> member(El, T);
member(_, []) -> false.

head([H|_]) -> H.

nth(1, [H|_]) -> H;
nth(N, [_|T]) when N>1 ->
    nth(N-1, T).

nthtail(1, [_|T]) -> T;
nthtail(N, [_|T]) when N>1 ->
    nthtail(N-1, T).

partition(Pred, List) when is_function(Pred, 1) ->
    partition(Pred, List, {[],[]}).

partition(_, [], {True, False}) ->
    {reverse(True), reverse(False)};
partition(Pred, [H|T], {True, False}) ->
    case Pred(H) of
        true ->  partition(Pred, T, {[H|True], False});
        false -> partition(Pred, T, {True, [H|False]})
    end.


prefix([H|T1], [H|T2]) -> prefix(T1, T2);
prefix([_|_], [_|_]) -> false;
prefix([], L) when is_list(L) -> true;
prefix(_, []) -> false.

suffix(L1, L2) -> prefix(reverse(L1), reverse(L2)).

seq(To) -> seq(1, To).
seq(From, To) ->
    seq(From, To, 1).
seq(From, To, Step)
    when is_integer(From), is_integer(To), is_integer(Step) ->
    do_seq(From, To, Step).

do_seq(From, To, Step) when Step>0, To>=From ->
    r_seq(From, To, Step, []);
do_seq(From, To, Step) when Step<0, To=<From ->
    l_seq(From, To, Step, []);
do_seq(From, _, 0) -> [From].

r_seq(From, To, Step, Acc) when (From+Step)>To ->
    reverse([From|Acc]);
r_seq(From, To, Step, Acc) ->
    r_seq(From+Step, To, Step, [From|Acc]).

l_seq(From, To, Step, Acc) when (From+Step)<To ->
    reverse([From|Acc]);
l_seq(From, To, Step, Acc) ->
    l_seq(From+Step, To, Step, [From|Acc]).



split(N, Lst) when is_integer(N), N>=0 -> split(N, [], Lst).
split(0, Fst, Snd) -> {reverse(Fst), Snd};
split(N, Fst, [H|T]) -> split(N-1, [H|Fst], T).

splitwith(Pred, List) -> {takewhile(Pred, List), dropwhile(Pred, List)}.

%sort(List) -> sort(fun(A,B) -> A=<B end, List).
%sort(Fun, _List) when is_function(Fun, 1) -> notimplemented.

sublist(L, Len) -> sublist(L, 1, Len).
sublist(L, Start, Len)
    when is_list(L), Len>=0, Start=<erlang:length(L) ->
    reverse(do_subl(L, Start, Len, [])).

do_subl([], _, _, Acc) -> Acc;
do_subl(_, _, 0, Acc) -> Acc;
do_subl([H|T], Start, Len, Acc) ->
    %io:format("~p~n", [Acc]),
    do_subl(T, Start+1, Len-1, [H|Acc]).

subtract(L1, L2) -> do_subtr(L1, L2, []).
do_subtr(L, [], Acc) -> reverse(Acc) ++ L;
do_subtr([], _, Acc) -> reverse(Acc);
do_subtr([H|T1], [H|T2], Acc) -> do_subtr(T1, T2, Acc);
do_subtr([H|T], L, Acc) -> do_subtr(T, L, [H|Acc]).

sum(Lst) -> foldl(fun(X,Y) -> X+Y end, 0, Lst).

zip(L1, L2) ->
    zipwith(fun(X, Y) -> {X, Y} end, L1, L2).

zip3(L1, L2, L3) ->
    zipwith3(
        fun(X, Y, Z) -> {X, Y, Z} end,
        L1, L2, L3
        ).

zipwith(Fun, L1, L2) when is_function(Fun, 2) ->
    do_zipwith(Fun, L1, L2, []).
do_zipwith(F, [H1|T1], [H2|T2], Acc) ->
    do_zipwith(F, T1, T2, [F(H1, H2)|Acc]);
do_zipwith(_, [], [], Acc) ->
    reverse(Acc).

zipwith3(Fun, L1, L2, L3) when is_function(Fun, 3) ->
    do_zipwith3(Fun, L1, L2, L3, []).
do_zipwith3(F, [H1|T1], [H2|T2], [H3|T3], Acc) ->
    do_zipwith3(F, T1, T2, T3, [F(H1, H2, H3)|Acc]);
do_zipwith3(_, [], [], [], Acc) ->
    reverse(Acc).

