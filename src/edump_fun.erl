-module('edump_fun').

%% API exports
-export([parse/1
        ]).

%%====================================================================
%% API functions
%%====================================================================

parse(Data) ->
    edump_parse:kv_section(fun fun_kv/1, Data).


%%====================================================================
%% Internal functions
%%====================================================================

fun_kv({<<"Module">>, M}) ->
    {module, binary_to_atom(M, latin1)};
fun_kv({<<"Uniq">>, U}) ->
    {uniq, binary_to_integer(U)};
fun_kv({<<"Index">>, V}) ->
    {index, binary_to_integer(V)};
fun_kv({<<"Address">>, <<"0x", Addr:16/binary>>}) ->
    {address, binary_to_integer(Addr, 16)};
fun_kv({<<"Native_address">>, <<"0x", Addr:16/binary>>}) ->
    {native_address, binary_to_integer(Addr, 16)};
fun_kv({<<"Refc">>, U}) ->
    {refc, binary_to_integer(U)}.
