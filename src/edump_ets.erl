-module(edump_ets).

%% API exports
-export([parse/1]).

%%====================================================================
%% API functions
%%====================================================================

parse(Data) ->
    FlatInfo = lists:append(edump_parse:kv_section(fun parse_kv/1, Data)),
    case proplists:get_value(type, FlatInfo) of
        undefined ->
            [{type, set} | FlatInfo];
        _ ->
            FlatInfo
    end.

%%====================================================================
%% Internal functions
%%====================================================================

parse_kv({<<"Slot">>, V}) ->
    [{slot, binary_to_integer(V)}];
parse_kv({<<"Table">>, Atom}) ->
    [{table, Atom}];
parse_kv({<<"Name">>, Atom}) ->
    [{name, Atom}];
parse_kv({<<"Buckets">>, V}) ->
    [{buckets, binary_to_integer(binary:part(V,0,byte_size(V)-1))}];
parse_kv({<<"Objects">>, V}) ->
    [{objects, binary_to_integer(V)}];
parse_kv({<<"Words">>, V}) ->
    [{words, binary_to_integer(V)}];
parse_kv({<<"Ordered set (AVL tree), Elements">>, V}) ->
    [{type, ord_set},
     {elements, binary_to_integer(V)}].
