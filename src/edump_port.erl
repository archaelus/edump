-module('edump_port').

%% API exports
-export([parse/1]).

%%====================================================================
%% API functions
%%====================================================================

parse(Data) ->
    [parse_kv(edump_parse:kv_line(L))
     || L <- edump_parse:lines(Data)].

%%====================================================================
%% Internal functions
%%====================================================================

parse_kv({<<"Slot">>, V}) ->
    {slot, binary_to_integer(V)};
parse_kv({<<"Connected">>, Pid}) ->
    {connected, {proc, Pid}};
parse_kv({<<"Links">>, Pid}) ->
    {links, {proc, Pid}};
parse_kv({<<"Port controls linked-in driver">>, Driver}) ->
    {driver, Driver};
parse_kv({<<"Port controls external process">>, Process}) ->
    {driver, {spawn, Process}};
parse_kv({<<"Port is UNIX fd not opened by emulator">>, <<A, "/", B>>}) ->
    {driver, {unix, A, B}};
parse_kv({<<"Monitors">>, Data}) ->
    case re:run(Data, <<"\\((?<pid>[<>.0-9]+),(?<ref>#Ref[<>.0-9]+)\\)">>,
                [global, {capture, all_names, binary}]) of
        {match, MData} ->
            {monitors,
             [{{proc, Pid}, Ref}
              || [{ref, Ref}, {pid, Pid}] <- MData]}
    end.
