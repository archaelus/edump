-module(edump_scheduler).

%% API exports
-export([parse/1]).

%%====================================================================
%% API functions
%%====================================================================

%% =scheduler:3
%% Scheduler Sleep Info Flags: 
%% Scheduler Sleep Info Aux Work: 
%% Current Port: 
%% Run Queue Max Length: 0
%% Run Queue High Length: 0
%% Run Queue Normal Length: 0
%% Run Queue Low Length: 0
%% Run Queue Port Length: 0
%% Run Queue Flags: OUT_OF_WORK | HALFTIME_OUT_OF_WORK | NONEMPTY | UNKNOWN(134217728)
%% Current Process: <0.39.0>
%% Current Process State: Running
%% Current Process Internal State: ACT_PRIO_NORMAL | USR_PRIO_NORMAL | PRQ_PRIO_NORMAL | ACTIVE | RUNNING
%% Current Process Program counter: 0x00000000194cf850 (unknown function)
%% Current Process CP: 0x000000001a3bc458 (erl_eval:do_apply/6 + 376)
%% Current Process Limited Stack Trace:
%% 0x0000000019efcbe8:SReturn addr 0x1A37DE00 (shell:exprs/7 + 712)
%% 0x0000000019efcc00:SReturn addr 0x1A37D4E8 (shell:eval_exprs/7 + 168)
%% 0x0000000019efcc58:SReturn addr 0x1A37D0B8 (shell:eval_loop/3 + 592)
%% 0x0000000019efcc90:SReturn addr 0x18069BF8 (<terminate process normally>)

%% This section has a weird and irritating format,
%% so we have do a crazy ish stateful parse.

parse(Data) ->
    lists:reverse(parse_lines(edump_parse:lines(Data), [])).

%%====================================================================
%% Internal functions
%%====================================================================

parse_lines([], Acc) ->
    Acc;
parse_lines([<<"Current Process Limited Stack Trace:">> | Stack], Acc) ->
    [{current_process_limited_stack_trace, Stack} | Acc];
parse_lines([Line | Rest], Acc) ->
    Acc1 = [edump_parse:kv_line(Line) | Acc],
    parse_lines(Rest, Acc1).
