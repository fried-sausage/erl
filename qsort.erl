-module(qsort).
-export([main/1]).

main([]) -> [];
main([Pivot|Rest]) ->
    {Smaller, Larger} = partition(Pivot,Rest,[],[]),
    main(Smaller) ++ [Pivot] ++ main(Larger).

partition(_, [], Smaller, Larger) ->
    {Smaller, Larger};
partition(Pivot, [H|T], Smaller, Larger) when H > Pivot ->
    partition(Pivot, T, Smaller, [H|Larger]);
partition(Pivot, [H|T], Smaller, Larger) when H =< Pivot ->
    partition(Pivot, T, [H|Smaller], Larger).