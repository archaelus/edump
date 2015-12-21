-module('edump').

%% API exports
-export([open/1
        ,open/2
        ,reopen/1
        ,proc_graph/1
        ,try_parse/1
        ,try_parse/2
        ]).

%%====================================================================
%% API functions
%%====================================================================

open(CrashdumpFile) ->
    edump_idx:open(CrashdumpFile).

open(CrashdumpFile, Opts) ->
    edump_idx:open(CrashdumpFile, Opts).

reopen(Handle) ->
    edump_idx:reopen(Handle).

proc_graph(Handle) ->
    edump_proc_graph:from_handle(Handle).

try_parse(Dump) ->
    try_parse(any, Dump).

try_parse(Type, Dump) when is_list(Dump) ->
    try_parse(Type, open(Dump));
try_parse(Type, Dump) ->
    edump_seg:first_parse_failure(Type, Dump).

%%====================================================================
%% Internal functions
%%====================================================================
