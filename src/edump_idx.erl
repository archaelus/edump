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
        ,find_by_id/2
        ,find_by_ids/2
        ,segments_of_type/2
        ,read_by_id/2
        ,read_by_ids/2
        ,read_seg/2
        ]).

-include("edump_seg.hrl").

-record(pos, {f :: file:fd(),
              file_offset :: pos_integer(),
              buffer = <<>> :: binary(),
              seg_re}).

-record(index, {crashdump_file :: string(),
                segments = [] :: [#seg{}]}).

-record(handle, {fd :: file:fd(), % fd to crashdump file
                 index_file :: file:name(),
                 crashdump_file :: file:name(),
                 index :: #index{}}).

-record(index_file, {vsn, index}).

-opaque handle() :: #handle{}.
-export_type([handle/0]).

%%====================================================================
%% API functions
%%====================================================================

open(CrashdumpFile) ->
    open(CrashdumpFile, default_options()).

default_options() ->
    #{write_index => true,
      index_checking => full,
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
            handle_from_index(CrashdumpFile, IndexFile, Index);
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

seg_id(#seg{id = ID}) -> ID.

find_by_id(Id, Index) ->
    lists:keyfind(Id, #seg.id, segments(Index)).

find_by_ids(Ids, Index) ->
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
     || Seg <- find_by_ids(Ids, Index)].

read_by_id(Id, Index) ->
    read_seg(find_by_id(Id, Index), Index).

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
    file:write_file(File, erlang:term_to_binary(Data)).

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

index_file(#index{crashdump_file=File}) -> File.
handle_index(#handle{index=Index}) -> Index.
handle_fd(#handle{fd=Fd}) -> Fd.

segment_type(SegID) ->
    case binary:split(SegID, <<":">>) of
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

first_block(File) ->
    {ok, F} = open_raw(File),
    block_init(F).

block_fd(#pos{f = F}) -> F.

block_init(F) ->
    {ok, RE} = re:compile("^=(?<seg>.*)(?<nl>\n?)", [multiline]),
    next_block(#pos{f = F,
                    file_offset=0,
                    seg_re = RE}, <<>>).

next_block(#pos{f = F} = LastBlock, RemainingBuffer) ->
    NextBlock = buf_end(LastBlock),
    NewOffset = NextBlock - byte_size(RemainingBuffer),
    case file:pread(F, NextBlock, 4096) of
        {ok, Buf} ->
            {case byte_size(Buf) of
                 4096 -> ok;
                 _ -> last_block
             end,
             LastBlock#pos{buffer = <<RemainingBuffer/binary,
                                      Buf/binary>>,
                           file_offset = NewOffset}};
        eof ->
            {last_block, LastBlock#pos{buffer = RemainingBuffer,
                                       file_offset = NewOffset}};
        {error, _} = Err ->
            Err
    end.

buf_end(#pos{file_offset = Start, buffer = Buf}) ->
    Start + byte_size(Buf).

buf_part(#pos{buffer=Buf, file_offset=Offset}, From) ->
    BufFrom = From - Offset,
    Len = byte_size(Buf) - BufFrom,
    binary:part(Buf, BufFrom, Len).

block_re(#pos{file_offset=_Off, buffer=Buf, seg_re=RE}) ->
    re:run(Buf, RE, [global, {capture, all_names, index}]).

%% Read Block
%% Scan for segment marks
%% Scan for newlines after Marks
%% If no newline after last mark, truncate block to [last_mark..end], recurse

build_index(Block) ->
    build_index(Block, []).

build_index({ok, Block}, Segments) ->
    case add_to_index(scan_segs(Block), Segments) of
        [#seg{id=incomplete_header, seg_start=SegStart}|NewSegs] ->
            Remaining = buf_part(Block, SegStart),
            build_index(next_block(Block, Remaining), NewSegs);
        NewSegs ->
            build_index(next_block(Block, <<>>), NewSegs)
    end;
build_index({last_block, Block}, Segments) ->
    NewSegs = add_to_index(scan_segs(Block), Segments),
    IndexSegs = lists:reverse(fix_last_seg(buf_end(Block), NewSegs)),
    {index, block_fd(Block), #index{segments = IndexSegs}};
build_index({error, _} = E, _) ->
    E.



scan_segs(Block) ->
    case block_re(Block) of
        {match, SegHeaders} ->
            match_to_segs(Block, SegHeaders);
        nomatch ->
            []
    end.

match_to_segs(Pos, SegHeaders) ->
    [match_to_seg(Pos, Header)
     || Header <- SegHeaders].

match_to_seg(#pos{buffer = Buf, file_offset=Off},
            [{NLPos,1},{SegIdStart, Len}]) ->
    SegFileStart = Off+SegIdStart-1,
    DataFileStart = Off+NLPos+1,
    #seg{id = segment_type(binary:part(Buf, SegIdStart, Len)),
         seg_start = SegFileStart,
         data_start = DataFileStart};
match_to_seg(#pos{file_offset=Off},
            [{_NLPos,0},{SegIdStart, _Len}]) ->
    #seg{id = incomplete_header,
         seg_start = Off+SegIdStart-1}.

%% Seg starts are known ("\n=" boundaries), but seg ends aren't, so
%% when we add a seg to the index, we fixup the last segment now that
%% we know its end. Segments must be added in the order they appear in
%% the file.
add_to_index(Segs, IDX) ->
    lists:foldl(fun add_seg_to_index/2,
                IDX,
                Segs).

add_seg_to_index(Seg = #seg{}, []) ->
    [Seg];
add_seg_to_index(Seg = #seg{seg_start=StartNext}, Idx) ->
    [Seg | fix_last_seg(StartNext, Idx)].

fix_last_seg(_, [#seg{data_start=undefined}|_] = Segs) ->
    %% Empty segment already fixed
    Segs;
fix_last_seg(StartNext,
             [#seg{seg_end=End}|_] = Segs)
  when is_integer(End), End + 1 =:= StartNext ->
    %% Segment already correct
    Segs;
fix_last_seg(StartNext,
             [Prev = #seg{data_start = StartNext,
                          seg_end = unknown} | Segs])
  when is_integer(StartNext) ->
    %% Fix empty length segment
    [Prev#seg{data_start=undefined,
              seg_end=StartNext-1} | Segs];
fix_last_seg(StartNext,
             [Prev = #seg{data_start=Start, seg_end=unknown} | Segs])
  when is_integer(Start), is_integer(StartNext),
       StartNext > Start ->
    %% Fix regular segment end
    [Prev#seg{seg_end=StartNext-1} | Segs].

my_vsn() ->
    proplists:get_value(vsn,?MODULE:module_info(attributes)).

crashdump(File, Opts) ->
    case build_index(first_block(File)) of
        {error, _} = E -> E;
        {index, Fd, Idx0} ->
            Index = Idx0#index{crashdump_file = File},
            check_index(Index, Fd, Opts)
    end.

check_index(Index, Fd, Opts = #{index_checking := by_size}) ->
    case file:position(Fd, eof) of
        {ok, Size} when Size < 1000000 ->
            check_index(Index, Fd, Opts#{index_checking => full});
        {ok, _TooBig} ->
            check_index(Index, Fd, Opts#{index_checking => none})
    end;
check_index(Index, Fd, #{index_checking := none}) ->
    {index, Fd, Index};
check_index(Index, Fd, #{index_checking := full}) ->
    run_checks(Index, Fd, [fun check_gaps/2,
                           fun check_readability/2]);
check_index(Index, Fd, Opts) ->
    check_index(Index, Fd, Opts#{index_checking => full}).


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
    io:format("Checking ~p segments...~n", [length(Segments)]),
    check_segs(Fd, Segments).

check_segs(_Fd, []) ->
    io:format("~n", []),
    ok;
check_segs(Fd, [S = #seg{} | Rest]) ->
    try raw_read_seg(S, Fd) of
        Data when is_binary(Data) ->
            io:format(".", []),
            check_segs(Fd, Rest);
        Else ->
    io:format("~n", []),
            {error, {seg_read_failed, S,
                     {not_binary, Else}}}
    catch
        Class:Ex ->
            io:format("~n", []),
            {error, {seg_read_failed, S,
                     {Class, Ex, erlang:get_stacktrace()}}}
    end.
