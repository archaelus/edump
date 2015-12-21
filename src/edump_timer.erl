-module('edump_timer').

%% API exports
-export([parse/1]).

%%====================================================================
%% API functions
%%====================================================================

parse(Data) ->
    edump_parse:kv_section(fun kv_line/1, Data).

%%====================================================================
%% Internal functions
%%====================================================================

kv_line({<<"Message">>, Message}) ->
    {message, binary_to_atom(Message, latin1)};
kv_line({<<"Time left">>, Rest}) ->
    case binary:split(Rest, <<" ">>) of
        [Time, <<"ms">>] ->
            {time_left, {binary_to_integer(Time), ms}};
        [Time] ->
            {time_left, {binary_to_integer(Time), ms}}
    end.
