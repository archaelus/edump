-module('edump_heap').

%% API exports
-export([parse/1
        ,parse_dict/1
        ,parse_msgs/1
        ,reconstruct/2
        ,reconstruct_dict/2
        ,reconstruct_msgs/2
        ,bin_refs/1
        ]).

%%====================================================================
%% API functions
%%====================================================================
-type addr() :: binary().
-type heap() :: [{addr(), edump_mem:value()}].
-spec parse(Data::binary()) -> heap().
parse(Data) ->
    [begin
         [Addr, Rest] = binary:split(Line, <<":">>),
         {Addr, edump_mem:parse(Rest)}
     end
     || Line <- edump_parse:lines(Data)].

parse_dict(Data) ->
    [ edump_mem:parse(Addr)
      || Addr <- edump_parse:lines(Data) ].

parse_msgs(Data) ->
    [begin
         [Addr, Rest] = binary:split(Line, <<":">>),
         {edump_mem:parse(Addr), edump_mem:parse(Rest)}
     end
     || Line <- edump_parse:lines(Data)].

reconstruct_msgs(Msgs, Heap) ->
    [{r(Ptr, Heap),
      r(Data, Heap)}
     || {Ptr, Data} <- Msgs].

reconstruct_dict(Dict, Heap) ->
    [reconstruct(Item, Heap)
     || Item <- Dict].

reconstruct(Ptr, Heap) ->
    r(Ptr, Heap).

r({cons, Head, Tail}, Heap) ->
    [r(Head, Heap) | r(Tail, Heap)];
r({tuple, List}, Heap) ->
    list_to_tuple([ r(Item, Heap)
                    || Item <- List ]);
r(nil, _Heap) ->
    [];
r({heap_ptr, Ptr}, Heap) ->
    case proplists:get_value(Ptr, Heap) of
        undefined ->
            {'$edump_bad_ptr$', Ptr};
        Value ->
            r(Value, Heap)
    end;
r({atom, Atom} = A, _Heap) ->
    try
        binary_to_existing_atom(Atom, latin1)
    catch
        error:badarg ->
            A
    end;
r({pid, What}, _Heap) ->
    {'$pid$', What};
r({port, What}, _Heap) ->
    {'$port$', What};
r({dist_external, BinTerm}, _Heap) ->
    edump_parse:term(BinTerm, unsafe_dist_external);
r(Else, _Heap) ->
    Else.

bin_refs(Heap) ->
    Refs = list_bin_refs([ Val || {_Addr, Val} <- Heap], ordsets:new()),
    [{binary, Ref} || Ref <- ordsets:to_list(Refs)].

%%====================================================================
%% Internal functions
%%====================================================================

list_bin_refs(List, Acc) ->
    lists:foldl(fun bin_ref/2, Acc, List).

bin_ref({sub_bin, {_Ptr, _Offset, _Length}} = Ref, Acc) ->
    ordsets:add_element(Ref, Acc);
bin_ref({refc_bin, {_Ptr, _Offset, _Length}} = Ref, Acc) ->
    ordsets:add_element(Ref, Acc);
bin_ref({cons, Head, Tail}, Acc) ->
    bin_ref(Tail, bin_ref(Head, Acc));
bin_ref({tuple, Slots}, Acc) ->
    list_bin_refs(Slots, Acc);
bin_ref(_, Acc) ->
    Acc.
