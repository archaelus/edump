-module('edump_analyse').

%% API exports
-export([proc_graph/3
        ,info/3
        ]).

%%====================================================================
%% API functions
%%====================================================================

proc_graph(CrashdumpFile, DotFile, Options) ->
    Graph = edump_proc_graph:from_handle(edump:open(CrashdumpFile)),
    edump_proc_dot:to_dot(DotFile,
                          Graph,
                          Options).

info(basic, Handle, _Opts) ->
    basic_info(edump:info({erl_crash_dump,<<"0.1">>}, Handle)),
    mem_info(edump:info(memory, Handle));
info(processes, Handle, Opts) ->
    Pids = edump_seg:ids_of_type(proc, Handle),
    Processes = [ {Pid, edump_seg:parse_id(Pid, Handle)}
                  || Pid <- Pids ],
    process_summary(Processes, Opts).

%%====================================================================
%% Internal functions
%%====================================================================

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

process_summary(Processes, Opts) ->
    Sort = maps:get(sort, Opts, pid),
    SortFn = case Sort of
                 pid -> fun p_sort_pid/2;
                 mem -> fun p_sort_mem/2;
                 msg_q -> fun p_sort_msg_q/2;
                 reds -> fun p_sort_reds/2
             end,
    Sorted = lists:sort(SortFn, Processes),
    Total = length(Processes),
    Len = case maps:get(max, Opts, infinity) of
              N when is_integer(N),
                     N < Total -> N;
              N when is_list(N) ->
                  list_to_integer(N);
              _ -> Total
          end,
    Procs = lists:zip(lists:sublist(Sorted, Len),
                      lists:seq(1, Len)),
    list_processes(Total, Procs),
    ok.

list_processes(NumProcesses, Procs) ->
    io:format("Processes (~p of ~p):~n", [NumProcesses, length(Procs)]),
    PosWidth = integer_to_list(length(integer_to_list(NumProcesses))),
    [ list_process(PosWidth, P)
      || P <- Procs ].

list_process(PosWidth, {{{proc, Pid}, Info}, Position}) ->
    State = proplists:get_value(state, Info),
    Reds = proplists:get_value(reductions, Info),
    Msgs = proplists:get_value(message_queue_length, Info),
    Bytes = proplists:get_value(stack_plus_heap, Info, "unknown"),
    PInfo = string:join([[State],
                         [integer_to_list(Bytes), " mem"],
                         [integer_to_list(Msgs), " msgq"],
                         [integer_to_list(Reds), " reds"]
                        ], ", "),
    Name = proplists:get_value(name, Info, ""),
    io:format("  ~"++PosWidth++"w ~10s ~20s (~s)~n",
              [Position, Pid, Name, PInfo]).

p_info_sort(Key, {_PidA, InfoA}, {_PidB, InfoB}) ->
    proplists:get_value(Key, InfoA) >
        proplists:get_value(Key, InfoB).

p_sort_mem(A, B) ->
    p_info_sort(stack_plus_heap, A, B).

p_sort_msg_q(A, B) ->
    p_info_sort(message_queue_length, A, B).

p_sort_reds(A, B) ->
    p_info_sort(reductions, A, B).

p_sort_pid({A, _}, {B, _}) ->
    A =< B.
