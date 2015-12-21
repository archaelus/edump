-module('edump_proc_dot').

%% API exports
-export([to_dot/2
        ,to_dot/3
        ,fewer_edges/3
        ]).

%%====================================================================
%% API functions
%%====================================================================

to_dot(File, DG) ->
    to_dot(File, DG, default_options()).

default_options() ->
    #{graph_attributes => ["rankdir=lr"]}.

to_dot(File, DG, Opts0) ->
    Opts = maps:merge(default_options(), Opts0),
    DotInfo = dot_info(DG, Opts),
    try proc_graph:render(DotInfo) of
        {ok, Str} ->
            file:write_file(File, Str);
        Else ->
            Else
    catch
        C:E ->
            erlang:error({C, E, DotInfo, erlang:get_stacktrace()})
    end.

fewer_edges({proc, _} = From, {proc, _} = To, link) ->
    From < To;
fewer_edges({port, _}, {proc, _}, link) ->
    true;
fewer_edges(_, erlang, spawned_by) ->
    true;
fewer_edges(_, _, spawned_by) ->
    false;
fewer_edges(_, _, _) ->
    true.

%%====================================================================
%% Internal functions
%%====================================================================

pids(DG) ->
    [ digraph:vertex(DG, V) || V <- digraph:vertices(DG) ].

relations(DG) ->
    [ begin
          {_, From, To, Label} = digraph:edge(DG, E),
          {From, To, Label}
      end
      || E <- digraph:edges(DG)].


pids_dot_info(DG, Opts) ->
    [ #{id => dot_id(Name),
        dot_attrs => dot_node_attributes(Name, Info)
       }
      || {Name, Info} <- pids(DG),
         include_node(Name, Info, Opts) ].

dot_info(DG, Opts = #{ graph_attributes := G }) ->
    #{ procs => pids_dot_info(DG, Opts),
       relations => relations_dot_info(DG, Opts),
       graph_attributes => G
     }.

relations_dot_info(DG, Opts) ->
    [ #{ from => dot_id(From),
         to => dot_id(To),
         dot_attrs => dot_edge_attributes(From, To, Label)
       }
      || {From, To, Label} <- relations(DG),
         include_edge(From, To, Label, Opts) ].

include_node(Node, Info, #{include_node := F})
  when is_function(F,2) ->
    F(Node, Info);
include_node(_, _, _) ->
    true.

include_edge(From, To, Label, #{include_edge := F})
  when is_function(F, 3) ->
    F(From, To, Label);
include_edge(_, _, _, _) ->
    true.



dot_id(T) ->
    pid_dot_label(T).


pid_dot_label({proc, ProcStr}) ->
    ProcStr;
pid_dot_label({port, PortStr}) ->
    PortStr;
pid_dot_label(erlang) ->
    "erlang".

%% DOT shapes: plaintext ellipse oval circle egg triangle box diamond
%% trapezium parallelogram house hexagon octagon note tab box3d
%% component
dot_node_attributes({proc, _} = Id, Info) ->
    Name = proc_name(Id, Info),
    lists:append([[io_lib:format("label=\"~s\"", [Name])],
                  case proplists:get_value(in_dump, Info, false) of
                      true -> [];
                      false -> ["style=dotted"]
                  end,
                  case proplists:get_value(state, Info) of
                      <<"Garbing">> -> ["color=red", "style=bold"];
                      <<"Exiting">> -> ["color=red", "style=dashed"];
                      <<"Running">> -> ["color=green"];
                      <<"Scheduled">> -> ["style=bold"];
                      _ -> []
                  end,
                  ["shape=oval"]]);
dot_node_attributes({port, Str}, _) ->
    [io_lib:format("label=\"~s\"", [Str]),
     "shape=box"];
dot_node_attributes(erlang, _) ->
    ["label=\"erlang\"",
     "shape=egg"].

dot_edge_attributes(From, To, Label) ->
    lists:append([format_label(Label),
                  dot_edge_style(From, To, Label)]).

dot_edge_style(_From, _To, {monitoring, _}) ->
    ["style=dashed"];
dot_edge_style(_From, _To, {monitored_by, _}) ->
    ["style=dashed"];
dot_edge_style(_, _, link) ->
    ["dir=none"];
dot_edge_style(_, _, _) ->
    [].

format_label(link) ->
    [];
format_label(L) ->
    [iolist_to_binary(["label=", $\", dot_edge_label(L), $\"])].

dot_edge_label(spawned_by) -> "spawned_by";
dot_edge_label(spawned) -> "spawned";
dot_edge_label({monitored_by, _}) -> "monitored_by";
dot_edge_label({monitoring, _}) -> "monitoring";
dot_edge_label(Label) ->
    io_lib:format("~p", [Label]).

proc_name({proc, Pid}, Info) ->
    case proplists:get_value(name,Info) of
        undefined ->
            Pid;
        Name ->
            [Name, $\n, Pid]
    end.
