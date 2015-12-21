-module('edump_dump').

%% API exports
-export([parse/1]).

%%====================================================================
%% API functions
%%====================================================================

%% Wed Oct  3 23:53:30 2012
%% Slogan: A test crash
%% System version: Erlang R15B01 (erts-5.9.1) [source] [64-bit] [smp:2:2] [async-threads:0] [hipe] [kernel-poll:false]
%% Compiled: Fri Apr 13 08:02:54 2012
%% Taints: 
%% Atoms: 7013

parse(Data) ->
    [Date | Lines] = edump_parse:lines(Data),
    [{date, Date}
     | [parse_line(L) || L <- Lines]].

%%====================================================================
%% Internal functions
%%====================================================================

parse_line(<<"Slogan: ", Slogan/binary>>) ->
    {slogan, Slogan};
parse_line(<<"System version: ", SystemVersion/binary>>) ->
    {system_version, SystemVersion};
parse_line(<<"Compiled: ", Date/binary>>) ->
    {compiled, Date};
parse_line(<<"Taints: ", Taints/binary>>) ->
    {taints, Taints};
parse_line(<<"Atoms: ", AtomsStr/binary>>) ->
    {atoms, binary_to_integer(AtomsStr)}.
