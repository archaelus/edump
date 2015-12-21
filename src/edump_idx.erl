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
        ,read_full_seg/2
        ,read_full_seg/3
        ,read_full/4
        ]).

-include("edump_seg.hrl").

-record(pos, {f :: file:fd(),
              file_offset :: pos_integer(),
              buffer = <<>> :: binary()}).

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

first_block(File) ->
    {ok, F} = open_raw(File),
    block_init(F).

block_fd(#pos{f = F}) -> F.

block_init(F) ->
    %% Construct a fake first block that pretends a \n exists before
    %% the first line. This allows us to always match \n= as the
    %% segment boundary.
    next_block(#pos{f = F,
                    file_offset=-1,
                    buffer = <<"\n">>}, <<"\n">>).

next_block(#pos{f = F} = LastBlock, RemainingBuffer) ->
    NextBlock = buf_end(LastBlock),
    NewOffset = NextBlock - byte_size(RemainingBuffer),
    case file:pread(F, NextBlock, ?READ_SIZE) of
        {ok, Buf} ->
            {case byte_size(Buf) of
                 ?READ_SIZE -> ok;
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

%% Read Block
%% Scan for segment marks
%% Scan for newlines after Marks
%% If no newline after last mark, truncate block to [last_mark..end], recurse

%% Build a list of segments in reverse order by scanning the crashdump file.
build_index(Block) ->
    build_index(Block, []).

build_index({ok, Block}, Segments) ->
    BlockSegs = scan_segs(Block),
    case add_to_index(BlockSegs, Segments) of
        [#seg{id=incomplete_header, seg_start=SegStart} | NewSegs] ->
            %% Remaining is seg_start (-1 so we include a newline)
            Remaining = buf_part(Block, SegStart-1),
            build_index(next_block(Block, Remaining), NewSegs);
        NewSegs ->
            build_index(next_block(Block, <<>>), NewSegs)
    end;
build_index({last_block, Block}, Segments) ->
    FileEnd = buf_end(Block),
    LastSegs = scan_segs(Block),
    [#seg{id=incomplete_header, seg_start=SegStart}| Rest] =
        add_to_index(LastSegs, Segments),
    SegStart = FileEnd-5,
    NewSegs = [end_seg(SegStart, FileEnd) | Rest],
    {index, block_fd(Block), #index{segments = lists:reverse(NewSegs)}};
build_index({error, _} = E, _) ->
    E.

add_to_index(Segs, Idx) when is_list(Segs), is_list(Idx) ->
    Segs ++ Idx.

end_seg(Start, End) ->
    #seg{id='end', seg_start=Start, data_start=undefined, seg_end=End-1}.

scan_segs(Block = #pos{buffer = Buffer}) ->
    case binary:matches(Buffer, <<"\n=">>) of
        [] -> [];
        SegMarkers ->
            match_to_segs(Block, SegMarkers, [])
    end.

%% No segments in block
%%
match_to_segs(_Block, [], Acc) -> Acc;
%%
%% Last segment header in a buffer - it's incomplete, so don't try and
%% find the header boundary (too many corner cases)
%% seg marker | header | end of block
%% seg marker | hea | end of block -- incomplete
%% seg marker | header | \n | end of block | data ...
%% seg marker | header | \n | end of block | = (new seg, prev is empty)
%%
match_to_segs(Block, [{Pos, 2}], Acc) ->
    SegStart = Pos + 1 + Block#pos.file_offset,
    [#seg{id=incomplete_header, seg_start=SegStart} | Acc];
%%
%% More than one segment header in the block - this means we know the
%% exact start and end for one of them, so we can parse correctly.
%%
%% Buffer diagram:
%% \n=header\ndata...\n=
%% ^ {Pos, 2} -- match start
%%   ^ -- seg start
%%    ^ Pos+2 -- seg header start
%%          ^ seg header end
%%          ^ nlpos
%%            ^ data start
%%                   ^ seg end
%%                   ^ next match start {NextPos, 2}
match_to_segs(Block, [{Header1Pos, 2} | [ {Header2Pos, 2} | _] = Rest ], Acc) ->
    FileOffset = Block#pos.file_offset,
    Seg1Start = Header1Pos+1, % (we consider seg start to be at the =...)
    Seg1End = Header2Pos, % the \n of the next segment
    %% Because we know the end of this segment, find_headerwill only
    %% fail on empty segments.
    Seg = case find_header(Block, Seg1Start+1, Header2Pos) of
              {ID, undefined} ->
                  #seg{id=ID,
                       seg_start = Seg1Start + FileOffset,
                       seg_end = Seg1End + FileOffset,
                       data_start = undefined};
              {ID, DataStart} when DataStart > Seg1Start,
                                   DataStart < Seg1End ->
                  #seg{id=ID,
                       seg_start = Seg1Start + FileOffset,
                       seg_end = Seg1End + FileOffset,
                       data_start = DataStart + FileOffset}
          end,
    match_to_segs(Block, Rest,
                  [ Seg | Acc ]).

-type buf_pos() :: pos_integer().
-spec find_header(#pos{}, From::buf_pos(), End::buf_pos()) ->
                         {segment_id(), PosAfterHeaderNL::buf_pos()}.
find_header(#pos{buffer=Buf}, From, End) ->
    case binary:match(Buf, <<"\n">>, [{scope, {From, End-From}}]) of
        {NLPos, 1} ->
            SegHeader = binary:part(Buf, From, NLPos-From),
            {segment_id(SegHeader), NLPos+1};
        nomatch ->
            {segment_id(binary:part(Buf, From, End-From)), undefined}
    end.

my_vsn() ->
    proplists:get_value(vsn,?MODULE:module_info(attributes)).

crashdump(File, Opts) ->
    case build_index(first_block(File)) of
        {error, _} = E -> E;
        {index, Fd, Idx0} ->
            Index = Idx0#index{crashdump_file = File},
            check_index(File, Index, Fd, Opts)
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
    io:format("Checking ~p segments...~n", [length(Segments)]),
    seg_readable(Fd, Segments).

seg_readable(_Fd, []) ->
    io:format("~n", []),
    ok;
seg_readable(Fd, [S = #seg{} | Rest]) ->
    try raw_read_seg(S, Fd) of
        <<"=", _/binary>> = Data->
            {error,
             {data_starts_with_seg_marker, S, Data}};
        Data when is_binary(Data) ->
            case check_for_marker(Data) of
                no_marker ->
                    io:format(".", []),
                    seg_readable(Fd, Rest);
                _ ->
                    {error, {seg_contains_marker, Data}}
            end;
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
    ,proc_stack
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
