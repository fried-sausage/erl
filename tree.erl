-module(tree).
-export([empty/0, insert/3, lookup/2]).

empty() -> {node, nil}.

%{node, {Key, Value, Smaller, Larger}}

insert(K, V, {node, nil}) ->
    {node,
        {K,
         V,
         {node, nil},
         {node, nil}
         }
    };
insert(NK, NV, {node, {K, V, Smaller, Larger}}) when (NK < K) ->
    {node,
        {K,
         V,
         insert(NK, NV, Smaller),
         Larger
         }
    };
insert(NK, NV, {node, {K, V, Smaller, Larger}}) when (NK > K) ->
    {node,
        {K,
         V,
         Smaller,
         insert(NK, NV, Larger)
         }
    };
insert(K, V, {node, {K, _, Smaller, Larger}}) ->
    {node,
        {K,
         V,
         Smaller,
         Larger
         }
    }.

lookup(_, {node, nil}) ->
    undefined;
lookup(Key, {node, {Key, Val, _, _}}) ->
    {ok, Val};
lookup(Key, {node, {Other, _, Smaller, _}}) when (Key < Other)->
    lookup(Key, Smaller);
lookup(Key, {node, {_, _, _, Larger}}) ->
    lookup(Key, Larger).
