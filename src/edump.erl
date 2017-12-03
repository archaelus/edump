-module('edump').

%% API exports
-export([h/0
        ,help/0
        ,open/1
        ,open/2
        ,reopen/1
        ,proc_graph/1
        ,try_parse/1
        ,try_parse/2
        ,pread/3
        ,processes/1
        ,ports/1
        ,info/2
        ,sort_procs/1
        ,sort_procs/2
        ]).

%% Escript export
-export([main/1]).

%%====================================================================
%% API functions
%%====================================================================

h() -> help().

-spec help() -> 'ok'.
help() ->
    io:put_chars(<<"Edump: File::filename(), H::edump_idx:handle(), "
                   "Digraph::digraph:graph(), Id::edump_seg:segment_id()\n"
                   "open(File)       -> H. Open a dump\n"
                   "open(File, Opts) -> H. Options write_index (true, bool())\n"
                   "                               index_checking "
                     "(by_size, by_size|full|cheap|none)\n"
                   "                               force_rebuild "
                     "(false, bool())\n"
                   "processes(H)     -> [Id]. Find all process segments\n"
                   "ports(H)         -> [Id]. Find all port segments\n"
                   "info(Id, H). Segment info for Id\n"
                   "sort_procs(H). \n"
                   "sort_procs(Opts, H). \n"
                   "reopen(H) -> H. \n"
                   "proc_graph(H) -> Digraph. \n"
                   "try_parse(File). Try to parse all segment types in File. "
                     "Stop on first error.\n"
                   "try_parse(SegType, File). "
                     "Parse only SegType segments in File.\n"
                   "pread(File, Start, End) -> binary(). "
                     "Read a block as a binary from File.\n"
                 >>).

open(CrashdumpFile) ->
    edump_idx:open(CrashdumpFile).

open(CrashdumpFile, Opts) ->
    edump_idx:open(CrashdumpFile,
                   maps:merge(edump_idx:default_options(), Opts)).

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

sort_procs(Handle) ->
    sort_procs(#{}, Handle).

sort_procs(Opts, Handle) ->
    edump_analyse:sort_procs(Handle, Opts).

%%====================================================================
%% Internal functions
%%====================================================================

main(Args) ->
    edump_script:main(Args).
