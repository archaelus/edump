-module('edump_proc').

%% API exports
-export([parse/1
        ,related_ids/1
        ,related_procs/1
        ,procs_in_index/1
        ,read/2
        ]).

%%====================================================================
%% API functions
%%====================================================================

parse(Data) when is_binary(Data) ->
    [parse_proc_data(L) || L <- edump_parse:lines(Data)].

related_ids({proc, Id}) ->
    [{proc_stack, Id},
     {proc_heap, Id},
     {proc_dictionary, Id},
     {ets, Id}].

related_procs(Info) ->
    lists:append([spawned_by(Info),
                  links(Info),
                  monitored_by(Info)]).

spawned_by(Info) ->
    case proplists:get_value(spawned_by, Info) of
        undefined -> [];
        Who -> [{spawned_by, Who}]
    end.

links(Info) ->
    [{link, Who}
     || Who <- proplists:get_value(links, Info, []),
        tuple_size(Who) =:= 2].

monitored_by(Info) ->
    [{monitored_by, Who, Ref}
     || {monitor, Who, Ref}  <- proplists:get_value(links, Info, [])].

procs_in_index(Handle) ->
    [ edump_idx:seg_id(S) || S <- edump_idx:segs_of_type(proc, Handle) ].

read(Id, Handle) ->
    parse(edump_idx:read_by_id(Id, Handle)).

%% State: Waiting
%% Name: application_controller
%% Spawned as: erlang:apply/2
%% Spawned by: <0.2.0>
%% Started: Wed Oct  3 23:47:05 2012
%% Message queue length: 0
%% Number of heap fragments: 0
%% Heap fragment data: 0
%% Link list: [<0.0.0>, <0.34.0>, <0.8.0>]
%% Reductions: 7881
%% Stack+heap: 28657
%% OldHeap: 28657
%% Heap unused: 6305
%% OldHeap unused: 25599
%% Program counter: 0x0000000014673478 (gen_server:loop/6 + 264)
%% CP: 0x0000000000000000 (invalid)
%% arity = 0
parse_proc_data(<<"State: ", State/binary>>) ->
    {state, State};
parse_proc_data(<<"Name: ", Name/binary>>) ->
    {name, Name};
parse_proc_data(<<"Spawned as: ", As/binary>>) ->
    {spawned_as, As};
parse_proc_data(<<"Spawned by: []">>) ->
    {spawned_by, erlang};
parse_proc_data(<<"Spawned by: ", Pid/binary>>) ->
    {spawned_by, {proc, Pid}};
parse_proc_data(<<"Message queue length: ", Len/binary>>) ->
    {message_queue_length, binary_to_integer(Len)};
parse_proc_data(<<"Number of heap fragments: ", Frags/binary>>) ->
    {heap_fragments, binary_to_integer(Frags)};
parse_proc_data(<<"Heap fragment data: ", Data/binary>>) ->
    {heap_fragment_data, Data};
parse_proc_data(<<"Link list: ", Data/binary>>) ->
    {ok, Tokens, _} = edump_link_list:string(binary_to_list(Data)),
    {ok, Links} = edump_link_list_parser:parse(Tokens),
    {links, Links};
parse_proc_data(<<"Reductions: ", Reds/binary>>) ->
    {reductions, binary_to_integer(Reds)};
parse_proc_data(<<"Stack+heap: ", Mem/binary>>) ->
    {stack_plus_heap, binary_to_integer(Mem)};
parse_proc_data(<<"Program counter: ", PC/binary>>) ->
    [ProgramCounter,Rest] = binary:split(PC, <<" (">>),
    {cp, {ProgramCounter, binary:part(Rest, 0, byte_size(Rest)-1)}};
parse_proc_data(<<"CP: ", CP/binary>>) ->
    [CodePointer,Rest] = binary:split(CP, <<" (">>),
    {cp, {CodePointer, binary:part(Rest, 0, byte_size(Rest)-1)}};
parse_proc_data(<<"arity = ", Arity/binary>>) ->
    {arity, binary_to_integer(Arity)};
parse_proc_data(Line) ->
    case binary:split(Line, <<": ">>) of
        [A,B] -> {A, B};
        [A] -> {unknown, A}
    end.

%%====================================================================
%% Internal functions
%%====================================================================
