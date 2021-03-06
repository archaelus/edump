-module('edump_seg').

%% API exports
-export([format/2
        ,fmt/2
        ,data/2
        ,print_data/2
        ,parse_id/2
        ,parse_seg/2
        ,id/1
        ,ids_of_type/2
        ,id_info/2
        ,id_info/3
        ,id_for/2
        ,related/2
        ,related_ids/2
        ,type/1
        ,try_parse_all/2
        ,first_parse_failure/2
        ,segment_types/1
        ]).

-include("edump_seg.hrl").
-export_type([segment_id/0]).

%%====================================================================
%% API functions
%%====================================================================

id(#seg{id = ID}) -> ID.

id_pid({K, Pid})
  when K =:= proc;
       K =:= ets;
       K =:= proc_dictionary;
       K =:= proc_heap;
       K =:= proc_messages;
       K =:= proc_stack ->
    Pid.

id_info(Id, Handle) ->
    id_info(type(Id), Id, Handle).

id_info(proc, Id, Handle) ->
    {proc, parse_id(Id, Handle),
     [id_info(RelId, Handle)
      || RelId <- related_ids(Id, Handle)]};
id_info(proc_dictionary, Id, Handle) ->
    {proc_dictionary,
     edump_heap:reconstruct_dict(parse_id(Id, Handle),
                                 heap_for(Id, Handle))};
id_info(proc_messages, Id, Handle) ->
    {proc_messages,
     edump_heap:reconstruct_msgs(parse_id(Id, Handle),
                                 heap_for(Id, Handle))};
id_info(proc_stack, Id, Handle) ->
    {proc_stack,
     edump_stack:reconstruct(parse_id(Id, Handle),
                             heap_for(Id, Handle))};
id_info(Type, Id, Handle) ->
    {Type, parse_id(Id, Handle),
     []}.

heap_for(Id, Handle) ->
    parse_id(heap_id_for(Id), Handle).

heap_id_for(Id) ->
    {proc_heap, id_pid(Id)}.

id_for(Type, {_, Id}) ->
    {Type, Id}.

related({proc_heap, _} = Id, Handle) ->
    Heap = heap_for(Id, Handle),
    BinRefs = edump_heap:bin_refs(Heap),
    edump_idx:find_ids(BinRefs, Handle);
related({proc, _} = Id, Handle) ->
    edump_idx:find_ids(edump_proc:related_ids(Id), Handle).

related_ids(Id, Handle) ->
    [ id(S) || S <- related(Id, Handle) ].

type(#seg{} = Seg) ->
    type(id(Seg));
type({Type, _}) ->
    Type;
type(Type) ->
    Type.

ids_of_type(Type, Handle) ->
    [ id(S)
      || S <- edump_idx:segments_of_type(Type, Handle) ].

format(Segs, Handle) ->
    io:format("~s", [fmt_segs(Segs, Handle)]).

fmt_segs(Segs, Handle) when is_list(Segs) ->
    [ begin
          Data = edump_idx:read_seg(Seg, Handle),
          fmt(Seg, Data)
      end
      || Seg <- Segs ].

fmt(#seg{id = {Type,Info}}, Data) ->
    [io_lib:format("=~p:~p~n", [Type, Info]), Data, "\n"];
fmt(#seg{id = Type}, Data) ->
    [io_lib:format("=~p~n", [Type]), Data, "\n"].


parse_id(Id, Handle) ->
    parse_data(type(Id),
               edump_idx:read_by_id(Id, Handle)).

parse_seg(#seg{} = Seg, Handle) ->
    parse_data(type(Seg),
               edump_idx:read_seg(Seg, Handle)).

print_data(Id, Handle) ->
    io:format("~p~n~s~n", [Id, data(Id, Handle)]).

data(Id, Handle) ->
    edump_idx:read_by_id(Id, Handle).

try_parse_all(Type, Handle) ->
    [ try parse_seg(S, Handle) of
          Parse ->
              {parse, {id(S), S, Parse}}
      catch
          Class:Ex ->
              {parse_fail,
               {{id(S), S, edump_idx:read_seg(S, Handle)},
                {Class, Ex, erlang:get_stacktrace()}}}
      end
      || S <- edump_idx:segments_of_type(Type, Handle) ].

first_parse_failure(Type, Handle) ->
    case [ F || {parse_fail, F} <- try_parse_all(Type, Handle)] of
        [First | _] ->
            First;
        [] -> no_failures
    end.

segment_types(Handle) ->
    lists:usort([type(Seg) || Seg <- edump_idx:segments(Handle)]).

%%====================================================================
%% Internal functions
%%====================================================================

parse_data(_, not_present) ->
    not_present;
parse_data(erl_crash_dump, Data) ->
    edump_dump:parse(Data);
parse_data(memory, Data) ->
    edump_parse:atom_int_block(Data);
parse_data(hash_table, Data) ->
    edump_parse:atom_int_block(Data);
parse_data(index_table, Data) ->
    edump_parse:atom_int_block(Data);
parse_data(allocated_areas, Data) ->
    edump_allocator:parse_areas(Data);
parse_data(allocator, Data) ->
    edump_allocator:parse(Data);
parse_data(port, Data) ->
    edump_port:parse(Data);
parse_data(ets, Data) ->
    edump_ets:parse(Data);
parse_data(timer, Data) ->
    edump_timer:parse(Data);
parse_data(node, <<>>) ->
    [];
parse_data(no_distribution, <<>>) ->
    [];
parse_data(visible_node, Data) ->
    edump_node:parse(Data);
parse_data(hidden_node, Data) ->
    edump_node:parse(Data);
parse_data(not_connected, Data) ->
    edump_node:parse(Data);
parse_data(loaded_modules, Data) ->
    edump_modules:parse_loaded(Data);
parse_data(mod, Data) ->
    edump_modules:parse_mod(Data);
parse_data('fun', Data) ->
    edump_fun:parse(Data);
parse_data(proc, Data) ->
    edump_proc:parse(Data);
parse_data(proc_dictionary, Data) ->
    edump_heap:parse_dict(Data);
parse_data(proc_messages, Data) ->
    edump_heap:parse_msgs(Data);
parse_data(proc_heap, Data) ->
    edump_heap:parse(Data);
parse_data(proc_stack, Data) ->
    edump_stack:parse(Data);
parse_data(binary, Data) ->
    [edump_parse:len_hexbin(Data)];
parse_data(atoms, Data) ->
    [{atom, A} || A <- edump_parse:lines(Data)];
parse_data(scheduler, Data) ->
    edump_scheduler:parse(Data);
parse_data('end', <<>>) ->
    [].
