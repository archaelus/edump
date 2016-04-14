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
    [{date, Date} | [edump_parse:kv_line(L) || L <- Lines]].

%%====================================================================
%% Internal functions
%%====================================================================
