-module(edump_proc_graph).

%% API exports
-export([from_handle/1
        ,pids/1
        ,relations/1
        ]).

%%====================================================================
%% API functions
%%====================================================================

from_handle(Handle) ->
    Segs = edump_idx:segments_of_type(proc, Handle),
    build_graph(Segs, digraph:new(), Handle).

pids(DG) ->
    [ digraph:vertex(DG, V) || V <- digraph:vertices(DG) ].

relations(DG) ->
    [ begin
          {_, From, To, Label} = digraph:edge(DG, E),
          {From, To, Label}
      end
      || E <- digraph:edges(DG)].

%%====================================================================
%% Internal functions
%%====================================================================

build_graph([], DG, _) ->
    DG;
build_graph([Seg | Segs], DG, Handle) ->
    Id = edump_seg:id(Seg),
    Info = edump_seg:parse_seg(Seg, Handle),
    add_proc(Id, Info, DG),
    add_links(Id,
              edump_proc:related_procs(Info),
              DG),
    build_graph(Segs, DG, Handle).

add_proc(Id, Info, DG) ->
    digraph:add_vertex(DG, Id, [{in_dump, true} | Info]).

add_links(Id, Links, DG) ->
    [ add_link(Id, Link, DG) || Link <- Links ],
    ok.

add_link(Self, {link, Who}, DG) ->
    maybe_add_vertex(Who, DG),
    maybe_add_edge(Self, Who, link, DG);
%%    maybe_add_edge(Who, Self, link, DG);
add_link(Self, {spawned_by, Who}, DG) ->
    maybe_add_vertex(Who, DG),
    maybe_add_edge(Self, Who, spawned_by, DG);
%%    maybe_add_edge(Who, Self, spawned, DG);
add_link(Self, {monitored_by, Who, Ref}, DG) ->
    maybe_add_vertex(Who, DG),
    maybe_add_edge(Self, Who, {monitored_by, Ref}, DG);
add_link(Self, {monitoring, Who, Ref}, DG) ->
    maybe_add_vertex(Who, DG),
    maybe_add_edge(Self, Who, {monitoring, Ref}, DG).

maybe_add_vertex(Who, DG) ->
    case digraph:vertex(DG, Who) of
        false ->
            digraph:add_vertex(DG, Who, []);
        _ ->
            ok
    end.

maybe_add_edge(From, To, Label, DG) ->
    FromEdges = [ digraph:edge(DG, E)
                  || E <- digraph:out_edges(DG, From) ],
    case [E || E = {_,F, T, L} <- FromEdges,
               L =:= Label,
               F =:= From,
               T =:= To] of
        [] ->
            digraph:add_edge(DG, From, To, Label);
        [_] ->
            ok
    end.
