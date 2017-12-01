-module('edump_parse').

%% API exports
-export([lines/1
        ,kv_line/1
        ,atom_int/1
        ,atom_int_block/1
        ,int_list/1
        ,kv_section/2
        ,hexbin/1
        ,len_hexbin/1
        ,term/2
        ,term_hexbin/2
        ]).

%%====================================================================
%% API functions
%%====================================================================

lines(Data) ->
    [L || L <- binary:split(Data, <<"\n">>, [global]),
          L =/= <<>>].

kv_line(Line) ->
    [A, B] = binary:split(Line, <<": ">>),
    {A, B}.

atom_int(Line) ->
    {A, B} = kv_line(Line),
    {binary_to_atom(A, latin1), binary_to_integer(B)}.

atom_int_block(Data) ->
    [ atom_int(L) || L <- lines(Data) ].

int_list(L) ->
    [ binary_to_integer(I)
      || I <- binary:split(L, <<" ">>, [global]),
         I =/= <<>> ].

kv_section(LineParser, Data) when is_function(LineParser, 1) ->
    [ LineParser(kv_line(Line))
      || Line <- lines(Data) ].

hexbin(B) ->
    << << (binary_to_integer(Byte, 16)) >>
       || <<Byte:2/binary>> <= B >>.

len_hexbin(B) ->
    [Len,Bin] = binary:split(B, <<":">>),
    Length = binary_to_integer(Len, 16),
    Binary = hexbin(Bin),
    Length = byte_size(Binary),
    Binary.

%% @doc Try to parse a hex encoded external term format. Returns the
%% term if safe, otherwise ```{UnsafeTag, ETFBinary}'''.
term_hexbin(HexBin, UnsafeTag) ->
    term(hexbin(HexBin), UnsafeTag).

%% @doc Try to parse an external term format from a binary
%% string. Return either the term, or {UnsafeTag,
%% OriginalBinary}. This allows callers to safely process binary terms
%% and be able to distinguish good from bad terms.
term(BinTerm, UnsafeTag) ->
    try
        erlang:binary_to_term(BinTerm, [safe])
    catch
        error:badarg ->
            {UnsafeTag, BinTerm}
    end.


%%====================================================================
%% Internal functions
%%====================================================================
