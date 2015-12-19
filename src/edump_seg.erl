-module('edump_seg').

%% API exports
-export([format/2
        ,fmt/2
        ,data/2
        ,print_data/2
        ,parse_id/2
        ,parse_seg/2
        ,id/1
        ,type/1
        ,try_parse_all/2
        ,first_parse_failure/2
        ,segment_types/1
        ]).

-include("edump_seg.hrl").

%%====================================================================
%% API functions
%%====================================================================

id(#seg{id = ID}) -> ID.

type(#seg{} = Seg) ->
    type(id(Seg));
type({Type, _}) ->
    Type;
type(Type) ->
    Type.

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

parse_data(proc, Data) ->
    edump_proc:parse(Data);
parse_data(proc_heap, Data) ->
    edump_heap:parse(Data);
parse_data(proc_stack, Data) ->
    edump_stack:parse(Data);
parse_data(Type, Data) ->
    {cant_parse, Type, Data}.
