-module('edump_proc_dot').

%% API exports
-export([to_dot/2]).

%%====================================================================
%% API functions
%%====================================================================

to_dot(File, DG) ->
    DotInfo = dot_info(DG),
    try proc_graph:render(DotInfo) of
        {ok, Str} ->
            file:write_file(File, Str);
        Else ->
            Else
    catch
        C:E ->
            erlang:error({C, E, DotInfo, erlang:get_stacktrace()})
    end.

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


pids_dot_info(DG) ->
    [ #{id => dot_id(Name),
        dot_attrs => dot_node_attributes(Name, Info)
       }
      || {Name, Info} <- pids(DG) ].

dot_info(DG) ->
    #{ procs => pids_dot_info(DG),
       relations => relations_dot_info(DG)
     }.

relations_dot_info(DG) ->
    [ #{ from => dot_id(From),
         to => dot_id(To),
         dot_attrs => dot_edge_attributes(From, To, Label)
       }
      || {From, To, Label} <- relations(DG),
         include(From, To, Label) ].

include({proc, From}, {proc, To}, link) when From < To -> true;
include({proc, _}, {proc, _}, link) -> false; % already drawn
include({proc, _From}, {port, _To}, link) -> true;
include({port, _}, {proc, _}, link) -> false; % already drawn
include(_, _, {monitored_by, _}) -> false;
include(_, _, spawned_by) -> false;
include(erlang, _, spawned) -> true;
include(_, _, spawned) -> false;
include(_, _, _) -> true.


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
dot_node_attributes({proc, Str}, Info) ->
    [io_lib:format("label=\"~s\"", [Str]),
     "shape=oval"];
dot_node_attributes({port, Str}, _) ->
    [io_lib:format("label=\"~s\"", [Str]),
     "shape=box"];
dot_node_attributes(erlang, _) ->
    ["label=\"erlang\"",
     "shape=egg"].

dot_edge_attributes(From, To, Label) ->
    lists:append([dot_edge_label(Label),
                  dot_edge_style(From, To, Label)]).

dot_edge_style(_From, _To, Label={monitoring, _}) ->
    ["style=dashed"];
dot_edge_style(_, _, Label = link) ->
    ["dir=none"];
dot_edge_style(_, _, Label) ->
    [].

dot_edge_label(link) -> "label=\"\"";
dot_edge_label(spawned_by) -> "label=\"spawned_by\"";
dot_edge_label(spawned) -> "label=\"spawned\"";
dot_edge_label({monitored_by, _}) -> "label=\"monitored_by\"";
dot_edge_label({monitoring, _}) -> "label=\"monitoring\"";
dot_edge_label(Label) ->
    io_lib:format("label=\"~p\"", [Label]).

%% link_graph(DG) ->
%%     DG2 = digraph:new(),
%%     [ begin
%%           copy_vertex(From, DG, DG2),
%%           copy_vertex(To, DG, DG2),
%%           digraph:add_edge(DG2, From, To, link)
%%       end
%%       || {_, From, To, link} E <- digraph:vertices(DG) ]

%% supervision_tree(DG) ->
%%     Tree = digraph:new(),
%%     copy_vertex(erlang, DG, Tree), %% special
%%     copy_tree(DG, Tree,
%%               {proc, <<"<0.0.0>">>},
%%               fun ({_, _From, _To, link}) ->
%%                       true;
%%                   (_) ->
%%                       false
%%               end),
%%     .

%% copy_tree(From, To, Start, EdgeFilter) ->
%%     case 
%%     Edges = filter_out_edges(From, Start, EdgeFilter),
    

%% filter_out_edges(G, V, F) ->
%%     [ E || E <- out_edges(G, V),
%%            F(E) ].

%% out_edges(G, V) ->
%%     [digraph:edge(G, E) || E <- out_edges(G, V)]

%% copy_vertex(V, DG1, DG2) ->
%%     {_, Label} = digraph:vertex(DG1, V),
%%     case digraph:vertex(DG2, V) of
%%         false ->
%%             digraph:add_vertex(DG2, V, Label);
%%         _ ->
%%             ok
%%     end.
