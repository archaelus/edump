-module('edump_idx').

%% API exports
-export([open/1
        ,open/2
        ,reopen/1
        ,close/1
        ,to_file/1
        ,to_file/2
        ,segments/1
        ,seg_id/1
        ,seg_size/1
        ,find_id/2
        ,find_ids/2
        ,segments_of_type/2
        ,read_by_id/2
        ,read_by_ids/2
        ,read_seg/2
        ,read_full_seg/2
        ,read_full_seg/3
        ,read_full/4
        ]).

-include("edump_seg.hrl").

-record(index, {crashdump_file :: string(),
                segments = [] :: [#seg{}]}).

-record(handle, {fd :: file:fd(), % fd to crashdump file
                 index_file :: file:name(),
                 crashdump_file :: file:name(),
                 index :: #index{}}).

-record(index_file, {vsn, index}).

-opaque handle() :: #handle{}.
-export_type([handle/0]).

-define(READ_SIZE, 16384).

%%====================================================================
%% API functions
%%====================================================================

open(CrashdumpFile) ->
    open(CrashdumpFile, default_options()).

default_options() ->
    #{write_index => true,
      index_checking => by_size,
      force_rebuild => false}.

open(CrashdumpFile, Opts = #{force_rebuild := true}) ->
    case crashdump(CrashdumpFile, Opts) of
        {error, _} = E -> E;
        {index, Fd, Index} ->
            IndexFile = index_filename(CrashdumpFile, Opts),
            case maps:get(write_index, Opts, true) of
                true -> to_file(IndexFile, Index);
                false -> ok
            end,
            handle_from_index(CrashdumpFile, Fd, IndexFile, Index)
    end;
open(CrashdumpFile, Opts) ->
    case index_exists(CrashdumpFile, Opts) of
        {exists, Index, IndexFile} ->
            {ok, Fd} = open_raw(CrashdumpFile),
            case check_index(CrashdumpFile, Index, Fd, Opts) of
                {index, Fd, Index} ->
                    handle_from_index(CrashdumpFile, Fd, IndexFile, Index);
                {error, _} = Err ->
                    Err
            end;
        {no_index, IndexFile} ->
            open(CrashdumpFile, Opts#{force_rebuild => true,
                                      index_file => IndexFile})
    end.

close(#handle{fd = undefined} = H) ->
    H;
close(#handle{fd = Fd} = H) ->
    file:close(Fd),
    H#handle{fd = undefined}.

reopen(#handle{} = H) ->
    handle_from_handle(close(H)).

segments(#handle{} = H) ->
    segments(handle_index(H));
segments(#index{segments=Segs}) ->
    Segs.

read_seg(false, _) ->
    not_present;
read_seg(Seg = #seg{},
         #handle{} = H) ->
    raw_read_seg(Seg, handle_fd(H)).

seg_size(#seg{data_start = undefined}) ->
    0;
seg_size(#seg{data_start = Start,
              seg_end = End}) ->
    End - Start.

seg_size_full(#seg{seg_start = Start,
                   seg_end = End}) ->
    End - Start.

read_full_seg(S, H) ->
    read_full_seg(S, 0, H).

read_full_seg(#seg{seg_start = Start,
                   seg_end = End}, Slack, H) ->
    read_full(Start, End, Slack, H).

read_full(Start, End, Slack, #handle{} = H) ->
    IOStart = Start - Slack,
    IOEnd = End + Slack,
    file:pread(handle_fd(H), IOStart, IOEnd - IOStart).

seg_id(#seg{id = ID}) -> ID.

find_id(Id, Index) ->
    lists:keyfind(Id, #seg.id, segments(Index)).

find_ids(Ids, Index) ->
    find(fun (#seg{id=Id}) ->
                 lists:member(Id, Ids)
         end,
         Index).

find(Filter, Index) ->
    lists:filter(Filter, segments(Index)).

segments_of_type(any, Index) ->
    segments(Index);
segments_of_type(Type, Index) ->
    find(fun (#seg{id={T, _}}) when T =:= Type -> true;
             (#seg{id=T}) when T =:= Type -> true;
             (_) -> false
         end,
         Index).

read_by_ids(Ids, Index) ->
    [{seg_id(Seg), read_seg(Seg, Index)}
     || Seg <- find_ids(Ids, Index)].

read_by_id(Id, Index) ->
    read_seg(find_id(Id, Index), Index).

%%====================================================================
%% Internal functions
%%====================================================================

-spec index_exists(file:name(), boolean()) ->
                          {exists, #index{}} |
                          {no_index, file:name()}.
index_exists(CrashdumpFile, Opts) ->
    IndexFile = index_filename(CrashdumpFile, Opts),
    case file:read_file(IndexFile) of
        {ok, B} ->
            try fixup(erlang:binary_to_term(B),
                      maps:get(ignore_vsn_mismatch, Opts, true)) of
                rebuild ->
                    {no_index, IndexFile};
                Index ->
                    {exists, Index, IndexFile}
            catch
                error:enoent ->
                    {no_index, IndexFile}
            end;
        {error, enoent} ->
            {no_index, IndexFile}
    end.

index_filename(_, #{index_file := File}) ->
    File;
index_filename(CrashdumpFile, _Opts) ->
    CrashdumpFile ++ ".eidx".

to_file(Idx) ->
    to_file(index_file(Idx) ++ ".eidx", Idx).

to_file(File, Idx) ->
    Data = #index_file{vsn = my_vsn(),
                       index = Idx},
    file:write_file(File, erlang:term_to_binary(Data, [compressed])).

fixup(#index_file{vsn = FileVSN, index=Idx}, IgnoreMismatch) ->
    case my_vsn() of
        FileVSN ->
            Idx;
        Wrong when IgnoreMismatch ->
            fixup_idx(Wrong, Idx);
        _Wrong ->
            rebuild
    end;
fixup(_, _) ->
    rebuild.

fixup_idx(_Vsn, Idx = #index{}) -> Idx;
fixup_idx(_, _) -> rebuild.

handle_from_handle(#handle{fd = undefined,
                           crashdump_file = CrashdumpFile,
                           index_file = IndexFile,
                           index= Index}) ->
    handle_from_index(CrashdumpFile, IndexFile, Index).

handle_from_index(CrashdumpFile, IndexFile, Index) ->
    {ok, CdFd} = open_raw(CrashdumpFile),
    handle_from_index(CrashdumpFile, CdFd, IndexFile, Index).

handle_from_index(CrashdumpFile, CdFd, IndexFile, Index) ->
    #handle{crashdump_file = CrashdumpFile,
            index_file = IndexFile,
            index = Index,
            fd = CdFd}.

handle_from_index(Index = #index{crashdump_file=File}, FD) ->
    #handle{fd = FD,
            crashdump_file = File,
            index = Index}.

index_file(#index{crashdump_file=File}) -> File.
handle_index(#handle{index=Index}) -> Index.
handle_fd(#handle{fd=Fd}) -> Fd.

-type segment_id () :: {mod, atom()} | {atom(), any()} | atom().
-spec segment_id(binary()) -> segment_id().
segment_id(Bin) ->
    case binary:split(Bin, <<":">>) of
        [<<"mod">>, Name] -> {mod, binary_to_atom(Name, latin1)};
        [A,B] -> {binary_to_atom(A, latin1), B};
        [A] -> binary_to_atom(A, latin1)
    end.

raw_read_seg(#seg{data_start=undefined}, _) ->
    <<>>; % Empty segment
raw_read_seg(#seg{data_start=Start, seg_end=End}, Fd)
  when is_integer(End), is_integer(Start), End >= Start ->
    {ok, D} = file:pread(Fd, Start, End-Start),
    D.

open_raw(File) ->
    file:open(File, [raw, binary, read,
                     read_ahead]).

%% Build a list of segments in reverse order by scanning lines in the
%% crashdump file.
build_index(FD, Offset, Acc) ->
    case file:read_line(FD) of
        {ok, <<"=", HeaderLine/binary>> = Line} ->
            NewOffset = byte_size(Line)+Offset,
            Header = binary:part(HeaderLine, 0, byte_size(HeaderLine)-1),
            Seg = #seg{id=segment_id(Header),
                       seg_start=Offset,
                       data_start=unknown,
                       seg_end=unknown},
            build_index(FD, NewOffset, [Seg | fix_seg_end(Offset, Acc)]);
        {ok, Line} when is_binary(Line) ->
            NewOffset = byte_size(Line) + Offset,
            build_index(FD, NewOffset, fix_data_start(Offset, Acc));
        eof ->
            Segs = lists:reverse(fix_seg_end(Offset, Acc)),
            {segs, Segs};
        {error, _} = E ->
            E
    end.

fix_seg_end(_, []) ->
    [];
fix_seg_end(Offset, [S = #seg{data_start=unknown, seg_end=unknown} | Rest]) ->
    [S#seg{data_start=undefined, seg_end=Offset-1} | Rest];
fix_seg_end(Offset, [S = #seg{seg_end=unknown} | Rest]) ->
    [S#seg{seg_end=Offset-1} | Rest].

fix_data_start(_Offset, [#seg{data_start=I}|_] = Acc)
  when is_integer(I) ->
    Acc;
fix_data_start(Offset, [S = #seg{data_start=unknown} | Rest]) ->
    [S#seg{data_start=Offset} | Rest].

my_vsn() ->
    proplists:get_value(vsn,?MODULE:module_info(attributes)).

crashdump(File, Opts) ->
    {ok, F} = open_raw(File),
    case build_index(F, 0, []) of
        {error, _} = E -> E;
        {segs, Segs} ->
            Index = #index{crashdump_file = File,
                           segments = Segs},
            check_index(File, Index, F, Opts)
    end.

check_index(CrashdumpFile, Index, Fd, Opts = #{index_checking := by_size}) ->
    case filelib:file_size(CrashdumpFile) of
        Small when Small < 10000000 ->
            check_index(CrashdumpFile, Index, Fd,
                        Opts#{index_checking => full});
        _TooBig ->
            check_index(CrashdumpFile, Index, Fd,
                        Opts#{index_checking => cheap})
    end;
check_index(_CrashdumpFile, Index, Fd, #{index_checking := none}) ->
    {index, Fd, Index};
check_index(_CrashdumpFile, Index, Fd, #{index_checking := Checkinglevel}) ->
    run_checks(Index, Fd, checks(Checkinglevel)).

checks(cheap) ->
    [fun check_gaps/2,
     fun check_seg_data/2,
     fun check_size/2];
checks(full) ->
    checks(cheap) ++
        [fun check_readability/2,
         fun check_parse/2].



run_checks(Index, Fd, []) ->
    {index, Fd, Index};
run_checks(Index, Fd, [F | Rest]) ->
    case F(Index, Fd) of
        ok ->
            run_checks(Index, Fd, Rest);
        Err ->
            Err
    end.

check_gaps(Index, _Fd) ->
    case gaps(Index) of
        [] -> ok;
        Gaps -> {error, {gaps, Gaps}}
    end.

gaps(#index{segments=Segs}) ->
    gaps(-1, Segs, []).

gaps(_, [], Acc) -> lists:reverse(Acc);
gaps(Pos, [#seg{seg_start=PosPlus1, seg_end=NewPos} | Segs], Acc)
  when Pos + 1 =:= PosPlus1 ->
    gaps(NewPos, Segs, Acc);
gaps(Pos, [#seg{seg_start=OtherPos, seg_end=NewPos} | Segs], Acc) ->
    gaps(NewPos, Segs, [{Pos, OtherPos} | Acc]).

check_readability(Index, Fd) ->
    Segments = segments(Index),
    seg_readable(Fd, Segments).

seg_readable(_Fd, []) ->
    ok;
seg_readable(Fd, [S = #seg{} | Rest]) ->
    try raw_read_seg(S, Fd) of
        <<"=", _/binary>> = Data->
            {error,
             {data_starts_with_seg_marker, S, Data}};
        Data when is_binary(Data) ->
            case check_for_marker(Data) of
                no_marker ->
                    seg_readable(Fd, Rest);
                _ ->
                    {error, {seg_contains_marker, Data}}
            end;
        Else ->
            {error, {seg_read_failed, S,
                     {not_binary, Else}}}
    catch
        Class:Ex ->
            {error, {seg_read_failed, S,
                     {Class, Ex, erlang:get_stacktrace()}}}
    end.

check_for_marker(Data) ->
    case binary:match(Data, <<"\n=">>) of
        nomatch ->
            no_marker;
        _ ->
            marker
    end.

check_seg_data(#index{segments=Segs}, _FD) ->
    case [ S
           || S <- Segs,
              bad_seg_data(S) ] of
        [] ->
            ok;
        BadSegs ->
            {error, {segs_with_bad_data, BadSegs}}
    end.

bad_seg_data(#seg{id = Id,
                  seg_start = Start,
                  data_start = undefined,
                  seg_end = End})
  when is_integer(Start),
       is_integer(End) ->
    lists:member(edump_seg:type(Id),
                 non_zero_length_types());
bad_seg_data(#seg{seg_start = Start,
                  data_start = Data,
                  seg_end = End})
  when is_integer(Start),
       is_integer(Data),
       is_integer(End) ->
    false;
bad_seg_data(_) ->
    true.

non_zero_length_types() ->
    [erl_crash_dump
    ,memory
    ,hash_table
    ,index_table
    ,allocated_areas
    ,allocator
    ,port
    ,ets
    ,timer
    ,visible_node
    ,not_connected
    ,loaded_modules
    ,mod
    ,'fun'
    ,proc
    ,proc_dictionary
    ,proc_messages
    ,atoms
    ].

check_size(Index, FD) ->
    Segments = segments(Index),
    Size = lists:foldl(fun (S, Acc) ->
                               seg_size_full(S) + Acc
                       end,
                       0,
                       Segments),
    NewLines = length(Segments),
    FullSize = NewLines + Size,
    case file:position(FD, eof) of
        {ok, FullSize} ->
            ok;
        {ok, Other} ->
            {error, {wrong_size,
                     [{segs, FullSize},
                      {missing, Other - FullSize},
                      {file, Other}]}};
        Else ->
            Else
    end.

check_parse(Index, FD) ->
    case edump_seg:first_parse_failure(any, handle_from_index(Index, FD)) of
        no_failures ->
            ok;
        F ->
            {error, F}
    end.
