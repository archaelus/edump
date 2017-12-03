%% Grammar for parsing Link list: line in proc segments.
Nonterminals
list list_elements link pid monitor registered_name.

Terminals
list_start list_sep list_end port ref proc atom node tuple_start tuple_sep tuple_end.

Rootsymbol list.

list -> list_start list_elements list_end : '$2'.

list_elements -> link : ['$1'].
list_elements -> link list_sep list_elements : ['$1'] ++ '$3'.

link -> monitor : '$1'.
link -> pid : {link, '$1'}.

pid -> proc : {proc, unwrap('$1')}.
pid -> port : {port, unwrap('$1')}.
pid -> registered_name : {registered, '$1'}.

monitor -> tuple_start atom tuple_sep pid tuple_sep ref tuple_end :
             {case unwrap('$2') of
                <<"to">> -> monitor;
                <<"from">> -> monitored_by
              end, '$4', unwrap('$6')}.

registered_name -> atom : unwrap('$1').
registered_name -> tuple_start atom tuple_sep node tuple_end :
                     {unwrap('$2'), unwrap('$4')}.

Erlang code.

unwrap({_,_,V}) -> V.
