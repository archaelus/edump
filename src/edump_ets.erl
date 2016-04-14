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
     {elements, binary_to_integer(V)}];
parse_kv({<<"Chain Length Avg">>, F}) ->
    [{chain_length_avg, binary_to_float(F)}];
parse_kv({<<"Chain Length Max">>, V}) ->
    [{chain_length_max, binary_to_integer(V)}];
parse_kv({<<"Chain Length Min">>, V}) ->
    [{chain_length_min, binary_to_integer(V)}];
parse_kv({<<"Chain Length Std Dev">>, F}) ->
    [{chain_length_stdev, binary_to_float(F)}];
parse_kv({<<"Chain Length Expected Std Dev">>, F}) ->
    [{chain_length_expected_stdev, binary_to_float(F)}];
parse_kv({<<"Fixed">>, A}) ->
    [{fixed, binary_to_existing_atom(A, latin1)}];
parse_kv({<<"Type">>, A}) ->
    [{type, binary_to_existing_atom(A, latin1)}];
parse_kv({<<"Protection">>, A}) ->
    [{protection, binary_to_existing_atom(A, latin1)}];
parse_kv({<<"Compressed">>, A}) ->
    [{compressed, binary_to_existing_atom(A, latin1)}];
parse_kv({<<"Write Concurrency">>, A}) ->
    [{write_concurrency, binary_to_existing_atom(A, latin1)}];
parse_kv({<<"Read Concurrency">>, A}) ->
    [{read_concurrency, binary_to_existing_atom(A, latin1)}];
parse_kv({K, V}) ->
    erlang:error({unknown_ets_info, {K, V}}).

