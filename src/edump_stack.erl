-module('edump_stack').

%% API exports
-export([parse/1
        ,heap_ptrs/1
        ,reconstruct/2
        ]).
-compile(export_all).

%%====================================================================
%% API functions
%%====================================================================

parse(Data) ->
    Lines = binary:split(Data, <<"\n">>, [global]),
    [parse_line(Line) || Line <- Lines,
                         byte_size(Line) > 0].

heap_ptrs(Stack) ->
    heap_ptrs(Stack, []).

heap_ptrs([], Acc) ->
    lists:usort(Acc);
heap_ptrs([{{var, _}, {heap, _} = H} | Rest], Acc) ->
    heap_ptrs(Rest, [H | Acc]);
heap_ptrs([{{var, _}, V} | Rest], Acc) ->
    heap_ptrs(Rest, process_mem:heap_ptrs(V, Acc));
heap_ptrs([_|Rest], Acc) ->
    heap_ptrs(Rest, Acc).

reconstruct(Stack, Heap) ->
    [ r(Item, Heap) || Item <- Stack ].

r({{var, _}, {'catch',_,_}} = I, _Heap) ->
    I;
r({{var, _} = V, Else}, Heap) ->
    {V, edump_heap:reconstruct(Else, Heap)};
r(Else, _Heap) ->
    Else.

%%====================================================================
%% Internal functions
%%====================================================================

parse_line(Line) ->
    case binary:split(Line, <<":">>) of
        [<<"y",Num/binary>>, Rest] ->
            parse_var(binary_to_integer(Num), Rest);
        [<<"0x", Addr:16/binary>>, Rest] ->
            parse_addr(binary_to_integer(Addr, 16), Rest)
    end.

parse_var(VarNum, <<"SCatch 0x", Addr2:8/binary, " ", Info/binary>>) ->
    {{var, VarNum}, {'catch', Addr2, Info}};
parse_var(VarNum, Rest) ->
    {{var, VarNum}, edump_mem:parse(Rest)}.

parse_addr(Addr, <<"SReturn addr 0x", Rest/binary>>) ->
    [Addr2, Info] = binary:split(Rest, <<" ">>),
    {{ptr, Addr}, {return_to, Addr2, Info}}.
