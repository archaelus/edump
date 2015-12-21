-module(edump_node).

%% API exports
-export([parse/1]).

%%====================================================================
%% API functions
%%====================================================================

parse(Data) ->
    edump_parse:kv_section(fun kv_line/1, Data).

%%====================================================================
%% Internal functions
%%====================================================================

kv_line({<<"Name">>, Name}) ->
    {name, binary_to_atom(Name, latin1)};
kv_line({<<"Creation">>, N}) ->
    {creation, binary_to_integer(N)};
kv_line({<<"Controller">>, <<"#Port",_/binary>> = Port}) ->
    {controller, {port, Port}};
kv_line({<<"Controller">>, <<"<",_/binary>> = Pid}) ->
    {controller, {proc, Pid}};
kv_line({<<"Remote link">>, Rest}) ->
    [From, To] = binary:split(Rest, <<" ">>),
    {remote_link, {proc, From}, {proc, To}};
kv_line({<<"Remote monitoring">>, Rest}) ->
    [From, To] = binary:split(Rest, <<" ">>),
    {remote_link, {proc, From}, {node, binary_to_atom(To, latin1)}}.
