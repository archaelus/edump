-module('edump_heap').

%% API exports
-export([parse/1
        ,parse_dict/1
        ,parse_msgs/1
        ]).

%%====================================================================
%% API functions
%%====================================================================

parse(Data) ->
    [begin
         [Addr, Rest] = binary:split(Line, <<":">>),
         {Addr, edump_mem:parse(Rest)}
     end
     || Line <- edump_parse:lines(Data)].

parse_dict(Data) ->
    [ edump_mem:parse(Addr)
      || Addr <- edump_parse:lines(Data) ].

parse_msgs(Data) ->
    [begin
         [Addr, Rest] = binary:split(Line, <<":">>),
         {edump_mem:parse(Addr), edump_mem:parse(Rest)}
     end
     || Line <- edump_parse:lines(Data)].


%%====================================================================
%% Internal functions
%%====================================================================
