-module('edump').

%% API exports
-export([open/1
        ,open/2
        ,reopen/1
        ,proc_graph/1
        ,try_parse/1
        ,try_parse/2
        ,pread/3
        ,processes/1
        ,ports/1
        ,info/2
        ,main/1
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

pread(File, Start, End) when is_list(File) ->
    {ok, FD} = file:open(File, [binary, raw]),
    Ret = file:pread(FD, Start, End-Start),
    file:close(FD),
    Ret.


processes(Handle) ->
    edump_seg:ids_of_type(proc, Handle).

ports(Handle) ->
    edump_seg:ids_of_type(port, Handle).

info(Id, Handle) ->
    edump_seg:id_info(Id, Handle).


main(Args) ->
    edump_script:main(Args).

%%====================================================================
%% Internal functions
%%====================================================================
