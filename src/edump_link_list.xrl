%% Lexer for the Link list: line in a crashdump proc segment.
Definitions.

P       = [0-9.]
D       = [0-9]
U       = [A-Z]
L       = [a-z]
A       = ({U}|{L}|{D}|_|@)

Rules.

\[           : {token,{list_start,TokenLine}}.
\]           : {token,{list_end,TokenLine}}.
,\s         : {token,{list_sep,TokenLine}}.
#Port<{P}+> : {token,{port,TokenLine,iolist_to_binary(TokenChars)}}.
#Ref<{P}+>  : {token,{ref,TokenLine,iolist_to_binary(TokenChars)}}.
%% I call pids procs throughout edump, this follows that convention.
<{P}+>      : {token,{proc,TokenLine,iolist_to_binary(TokenChars)}}.

{L}{A}*     : {token,{atom,TokenLine,iolist_to_binary(TokenChars)}}.
'(\\\^.|\\.|[^'])*' : Atom = string_gen(strip(TokenChars,TokenLen)),
                      case is_node(Atom) of
                        true -> {token, {node,TokenLine,iolist_to_binary(Atom)}};
                        false -> {token, {atom,TokenLine,iolist_to_binary(Atom)}}
                      end.
{           : {token,{tuple_start,TokenLine}}.
,           : {token,{tuple_sep,TokenLine}}.
}           : {token,{tuple_end,TokenLine}}.

Erlang code.

strip(TokenChars,TokenLen) ->
    lists:sublist(TokenChars, 2, TokenLen - 2).

is_node(String) ->
    case string:find(String, "@") of
        nomatch -> false;
        _ -> true
    end.

string_gen([$\\|Cs]) ->
    string_escape(Cs);
string_gen([C|Cs]) ->
    [C|string_gen(Cs)];
string_gen([]) -> [].

string_escape([O1,O2,O3|S]) when
  O1 >= $0, O1 =< $7, O2 >= $0, O2 =< $7, O3 >= $0, O3 =< $7 ->
    [(O1*8 + O2)*8 + O3 - 73*$0|string_gen(S)];
string_escape([$^,C|Cs]) ->
    [C band 31|string_gen(Cs)];
string_escape([C|Cs]) when C >= $\000, C =< $\s ->
    string_gen(Cs);
string_escape([C|Cs]) ->
    [escape_char(C)|string_gen(Cs)].

escape_char($n) -> $\n; %\n = LF
escape_char($r) -> $\r; %\r = CR
escape_char($t) -> $\t; %\t = TAB
escape_char($v) -> $\v; %\v = VT
escape_char($b) -> $\b; %\b = BS
escape_char($f) -> $\f; %\f = FF
escape_char($e) -> $\e; %\e = ESC
escape_char($s) -> $\s; %\s = SPC
escape_char($d) -> $\d; %\d = DEL
escape_char(C) -> C.
