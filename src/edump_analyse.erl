-module('edump_analyse').

%% API exports
-export([proc_graph/3
        ,info/2
        ]).

%%====================================================================
%% API functions
%%====================================================================

proc_graph(CrashdumpFile, DotFile, Options) ->
    Graph = edump_proc_graph:from_handle(edump:open(CrashdumpFile)),
    edump_proc_dot:to_dot(DotFile,
                          Graph,
                          Options).

info(basic, Handle) ->
    basic_info(edump:info({erl_crash_dump,<<"0.1">>}, Handle)),
    mem_info(edump:info(memory, Handle)).


basic_info({erl_crash_dump, Basic, _}) ->
    io:format("Crashed: ~p~n", [proplists:get_value(date, Basic)]),
    io:format("Slogan: ~s~n", [proplists:get_value(slogan, Basic)]),
    [ io:format("Tainted by: ~s~n", [T])
      || {taints, T} <- Basic,
         byte_size(T) > 0].

mem_info({memory, Mem0, _}) ->
    Total = proplists:get_value(total, Mem0),
    Scale = scale(Total),
    Mem = [{atom_to_list(A), V}
           || {A, V} <- Mem0,
              A =/= total],
    {Used, Section} =
        lists:partition(fun ({K, _V}) ->
                                string:str(K, "_used") =/= 0
                        end,
                        Mem),
    Descending = lists:reverse(lists:keysort(2, Section)),
    io:format("Total memory: ~sb~n", [scale(Total, Scale)]),
    [ case proplists:get_value(S ++ "_used", Used) of
          undefined ->
              io:format("  ~6.2f% ~s: ~sb~n",
                        [100 * Bytes / Total, S, scale(Bytes, Scale)]);
          UsedBytes ->
              io:format("  ~6.2f% ~s: ~sb (~6.2f% used)~n",
                        [100 * Bytes / Total, S,
                         scale(Bytes, Scale), 100 * UsedBytes/Bytes])
      end
      || {S, Bytes} <-  Descending ].

scale(Num, {Suffix, Divisor}) ->
    io_lib:format("~p~s", [Num/Divisor, Suffix]).

scale(G) when G > 1000000000 ->
    {"G", 1000000000};
scale(M) when M > 1000000 ->
    {"M", 1000000};
scale(K) when K > 1000 ->
    {"K", 1000};
scale(_) ->
    {"", 1}.



%%====================================================================
%% Internal functions
%%====================================================================
