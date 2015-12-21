-module('edump_script').

%% API exports
-export([main/1]).

%%====================================================================
%% API functions
%%====================================================================

main(["index" | Args]) ->
    case getopt:parse_and_check(index_options(), Args) of
        {ok, {IndexOpts, Args1}} ->
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
        {error, {missing_required_option, What}} ->
            io:format("Missing option: ~p~n", [What]),
            getopt:usage(index_options(), "edump index");
        {error, What} ->
            io:format("Problem with arguments: ~p~n", [What]),
            getopt:usage(index_options(), "edump index")
    end;
main(Args) ->
    getopt:usage(top_options(), "edump").

top_options() ->
    [
     %% {Name, ShortOpt, LongOpt, ArgSpec, HelpMsg}
     {help,     $h, "help",     undefined, "Print this help."},
     {version,  $v, "version",  undefined, "Show version information."},
     {task,     undefined, undefined, atom, "Task to run: index"}
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

%%====================================================================
%% Internal functions
%%====================================================================
