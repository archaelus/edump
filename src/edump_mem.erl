-module('edump_mem').

%% API exports
-export([parse/1
        ,heap_ptrs/2
        ]).

-type edump_atom() :: {'atom', Name::binary()}.
-type edump_pid() :: {'pid', edump_idx:seg_id()}.
-type edump_port() :: {'port', edump_idx:seg_id()}.
-type edump_heap_ptr() :: {'heap', Ptr::binary()}.
-type edump_cons() :: {'cons', Head::edump_types(), Tail::edump_types()}.
-type edump_types() :: edump_atom() |
                       edump_pid() |
                       edump_port() |
                       edump_heap_ptr() |
                       edump_tuple() |
                       edump_cons() |
                       'nil'.
-type edump_tuple() :: {'tuple', edump_types()}.


%%====================================================================
%% API functions
%%====================================================================

-spec parse(binary()) -> edump_types().
parse(<<"A", Rest/binary>>) ->
    {Len, Atom} = thing_len(Rest),
    parse_atom(Len, Atom);
parse(<<"H",HeapAddr/binary>>) ->
    {heap_ptr, HeapAddr};
parse(<<"t", Rest/binary>>) ->
    {Len, TupleData} = thing_len(Rest),
    parse_tuple(Len, TupleData);
parse(<<"I", IntStr/binary>>) ->
    binary_to_integer(IntStr);
parse(<<"B", BignumStr/binary>>) ->
    binary_to_integer(BignumStr);
parse(<<"F", Rest/binary>>) ->
    {_Len, FloatData} = thing_len(Rest),
    binary_to_float(FloatData);
parse(<<"P", Pid/binary>>) ->
    {pid, {proc, Pid}};
parse(<<"p", Port/binary>>) ->
    {port, {port, <<"#Port", Port/binary>>}};
parse(<<"l", Rest/binary>>) ->
    [Head, Tail] = binary:split(Rest, <<"|">>),
    {cons, parse(Head), parse(Tail)};
parse(<<"Yh", Rest/binary>>) ->
    case parse_bytes(Rest,bin,incomplete_binary) of
        {bin, B} -> B;
        Else -> Else
    end;
parse(<<"Ys", Rest/binary>>) ->
    [Val, Offset, Length] = binary:split(Rest, <<":">>, [global]),
    {sub_bin, Val, Offset, Length};
parse(<<"Yc", Rest/binary>>) ->
    [Val, Offset, Length] = binary:split(Rest, <<":">>, [global]),
    {refc_bin, Val, Offset, Length};
parse(<<"E", Rest/binary>>) ->
    parse_bytes(Rest, dist_external, broken_dist_external);
parse(<<"N">>) ->
    nil.

heap_ptrs({heap,_} = H, Acc) ->
    [H | Acc];
heap_ptrs({tuple, Entries}, Acc) ->
    lists:foldl(fun heap_ptrs/2,
                Acc,
                Entries);
heap_ptrs(_, Acc) ->
    Acc.


%%====================================================================
%% Internal functions
%%====================================================================

parse_tuple(0, <<>>) ->
    {tuple, []};
parse_tuple(Len, Rest) ->
    TupleEntries = binary:split(Rest, <<",">>, [global]),
    case length(TupleEntries) of
        Len ->
            {tuple, [parse(Entry)
                     || Entry <- TupleEntries]};
        BadLen ->
            erlang:error({bad, tuple, {Len, vs, BadLen},
                          Rest,
                          TupleEntries})
    end.

parse_atom(Len, Rest) ->
    {atom, binary:part(Rest, 0, Len)}.

parse_bytes(Data,CompleteTag,_IncompleteTag) ->
    {Len, Thing} = thing_len(Data),
    Bytes = << <<(binary_to_integer(B, 16)):8>>
               || <<B:2/binary>> <= Thing >>,
    case byte_size(Bytes) of
        Len ->
            {CompleteTag, Bytes};
        _ ->
            erlang:error({bad, CompleteTag, Data, Bytes})
    end.

thing_len(Data) ->
    [Size, Rest] = binary:split(Data, <<":">>),
    Len = binary_to_integer(Size, 16),
    {Len, Rest}.
