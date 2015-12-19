-module('edump_heap').

%% API exports
-export([parse/1
        ]).

%%====================================================================
%% API functions
%%====================================================================

parse(Data) ->
    Lines = binary:split(Data, <<"\n">>, [global]),
    [begin
         [Addr, Rest] = binary:split(Line, <<":">>),
         {Addr, edump_mem:parse(Rest)}
     end
     || Line <- Lines,
        byte_size(Line) > 0].

%%====================================================================
%% Internal functions
%%====================================================================
