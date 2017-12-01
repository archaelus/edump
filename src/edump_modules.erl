-module('edump_modules').

%% API exports
-export([parse_loaded/1
        ,parse_mod/1
        ]).

%%====================================================================
%% API functions
%%====================================================================

parse_loaded(Data) ->
    edump_parse:kv_section(fun loaded/1, Data).

parse_mod(Data) ->
    edump_parse:kv_section(fun mod/1, Data).

%%====================================================================
%% Internal functions
%%====================================================================

loaded({<<"Current code">>, Code}) ->
    {current_code, binary_to_integer(Code)};
loaded({<<"Old code">>, Old}) ->
    {old_code, binary_to_integer(Old)}.

mod({<<"Current size">>, Sz}) ->
    {current_size, binary_to_integer(Sz)};
mod({<<"Current attributes">>, Attr}) ->
    {current_attributes,
     edump_parse:term_hexbin(Attr, unsafe_attribute_info)};
mod({<<"Current compilation info">>, Info}) ->
    {current_compilation_info,
     edump_parse:term_hexbin(Info, unsafe_compilation_info)};
mod({<<"Old size">>, Sz}) ->
    {old_size, binary_to_integer(Sz)};
mod({<<"Old attributes">>, Attr}) ->
    {old_attributes,
     edump_parse:term_hexbin(Attr, unsafe_attribute_info)};
mod({<<"Old compilation info">>, Info}) ->
    {old_compilation_info,
     edump_parse:term_hexbin(Info, unsafe_compilation_info)}.
