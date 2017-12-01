-module('edump_script').

%% API exports
-export([main/1]).

%%====================================================================
%% API functions
%%====================================================================

main(["index" | Args]) ->
    case getopt:parse_and_check(index_options(), Args) of
        {ok, {IndexOpts, _Args1}} ->
            Dump = proplists:get_value(file, IndexOpts),
            Rebuild = proplists:get_value(rebuild, IndexOpts),
            Checking = proplists:get_value(checking, IndexOpts),
            io:format("Indexing ~p ~p~n", [Dump, IndexOpts]),
            case timer:tc(edump,open,
                          [Dump, #{force_rebuild => Rebuild,
                                   index_checking => Checking}]) of
                {_Time, {error, What}} ->
                    io:format("Couldn't index crashdump: ~p~n", [What]),
                    erlang:halt(1);
                {Time, _} ->
                    io:format("Indexed ~p in ~ps~n",
                              [Dump, Time/1000000])
            end;
        {error, What} ->
            getopt:format_error(index_options(), What),
            getopt:usage(index_options(), "edump index")
    end;
main(["graph" | Args]) ->
    case getopt:parse_and_check(graph_options(), Args) of
        {ok, {GraphOpts, _Args1}} ->
            Dump = proplists:get_value(file, GraphOpts),
            DotFile = proplists:get_value(dot_file, GraphOpts),
            Attrs = string:tokens(proplists:get_value(graph_attrs, GraphOpts),
                                  ";"),
            GOpts = #{graph_attributes => Attrs,
                      include_edge =>
                          case proplists:get_value(prune, GraphOpts) of
                              true ->
                                  fun edump_proc_dot:fewer_edges/3;
                              false ->
                                  undefined
                          end},
            io:format("Graph opts: ~p~n", [GOpts]),
            case edump_analyse:proc_graph(Dump, DotFile,
                                          GOpts) of
                ok ->
                    io:format("Wrote graph to ~p~n", [DotFile]);
                {error, What} ->
                    io:format("Couldn't write graph: ~p~n", [What]),
                    erlang:halt(1)
            end;
        {error, What} ->
            getopt:format_error(graph_options(), What),
            getopt:usage(graph_options(), "edump graph"),
            erlang:halt(1)
    end;
main(["info" | Args]) ->
    case getopt:parse_and_check(info_options(), Args) of
        {ok, {InfoOpts, Rest}} ->
            Dump = proplists:get_value(file, InfoOpts),
            Handle = edump:open(Dump),
            Kind = proplists:get_value(info, InfoOpts),
            io:format("Crashdump ~p~n", [Dump]),
            edump_analyse:info(Kind, Handle, maps:from_list(Rest)),
            ok;
        {error, What} ->
            getopt:format_error(info_options(), What),
            getopt:usage(info_options(), "edump info"),
            erlang:halt(1)
    end;
main(_Args) ->
    getopt:usage(top_options(), "edump"),
    erlang:halt(1).

top_options() ->
    [
     %% {Name, ShortOpt, LongOpt, ArgSpec, HelpMsg}
     {help,     $h, "help",     undefined, "Print this help."},
     {version,  $v, "version",  undefined, "Show version information."},
     {task,     undefined, undefined, atom, "Task to run: index, graph, info"}
    ].

index_options() ->
    [
     %% {Name, ShortOpt, LongOpt, ArgSpec, HelpMsg}
     {help,     $h, "help",     undefined, "Print this help."},
     {version,  $v, "version",  undefined, "Show version information."},
     {rebuild,  $f, "force-rebuild", {boolean, false},
      "Rebuild the index even if it exists."},
     {checking, $c, "checking", {atom, by_size},
      "Index checking level (none, cheap, full, by_size)"},
     {file,     undefined, undefined, string, "Crashdump file to index."}
    ].

graph_options() ->
    [
     %% {Name, ShortOpt, LongOpt, ArgSpec, HelpMsg}
     {help,     $h, "help",     undefined, "Print this help."},
     {version,  $v, "version",  undefined, "Show version information."},
     {graph_attrs, $g, "graph-attributes", {string, "rankdir=\"TB\";size=\"8,6\";root=\"erlang\";dpi=\"150\""},
     "Graph attributes in DOT format. Specify attributes as one string
     separated by ';', with no terminating ';'."},
     {prune,    $p, "prune",   {boolean, true}, "Prune the output to produce a tidier graph (shows fewer relations between entities in the dump"},
     {file,     undefined, undefined, string, "Crashdump file to graph."},
     {dot_file, $d, undefined, string, "Dotfile for graph output."}
    ].

info_options() ->
    [
     %% {Name, ShortOpt, LongOpt, ArgSpec, HelpMsg}
     {help,     $h, "help",     undefined, "Print this help."},
     {version,  $v, "version",  undefined, "Show version information."},
     {file,     undefined, undefined, string, "Crashdump file."},
     {info,     $i, "info",     {atom,basic}, "Kind of info to extract from crashdump. (e.g. basic, processes, ports, ...)"}
    ].

%%====================================================================
%% Internal functions
%%====================================================================
