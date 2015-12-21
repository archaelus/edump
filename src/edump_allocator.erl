-module('edump_allocator').

%% API exports
-export([parse_areas/1
        ,parse/1
        ]).

%%====================================================================
%% API functions
%%====================================================================

%% allocated_areas
parse_areas(Data) ->
    edump_parse:kv_section(fun area_line/1, Data).

%% allocator
parse(Data) ->
    [ allocator_line(edump_parse:kv_line(L))
      || L <- edump_parse:lines(Data) ].

%%====================================================================
%% Internal functions
%%====================================================================

area_line({K, V}) ->
    {binary_to_atom(K, latin1),
     case [binary_to_integer(I)
           || I <- binary:split(V, <<" ">>, [global])] of
         [I] -> I;
         L -> L
     end};
area_line(L) ->
    edump_parse:atom_int(L).

allocator_line({K, V}) ->
    Key = [binary_to_atom(Part, latin1)
           || Part <- binary:split(K, <<" ">>, [global])],
    {Key,
     values(Key, V)}.

values([version], V) ->
    V;
values([versions], V) ->
    binary:split(V, <<" ">>, [global]);
values([fix, type], V) ->
    [Thing, A, B] = space_split(V),
    [binary_to_atom(Thing, latin1),
     binary_to_integer(A),
     binary_to_integer(B)];
values([option, as], V) ->
    binary_to_atom(V, latin1);
values([memory, kind], V) ->
    V;
values(_, V) ->
    [ case I of
          <<"true">> -> true;
          <<"false">> -> false;
          <<"libc">> -> libc;
          I -> binary_to_integer(I)
      end
      || I <- space_split(V) ].

space_split(V) ->
    binary:split(V, <<" ">>, [global]).
