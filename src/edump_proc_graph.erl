-module(edump_proc_graph).

%% API exports
-export([from_handle/1
        ,pg_to_dot/1
        ,pids/1
        ,relations/1
        ]).

%%====================================================================
%% API functions
%%====================================================================

from_handle(Handle) ->
    Segs = edump_idx:segs_of_type(proc, Handle),
    build_graph(Segs, digraph:new(), Handle).

pg_to_dot(DG) ->
    Template = read_template("proc_graph.tmpl"),
    bbmustache:render(Template,
                      #{pids => pids(DG),
                        relations => relations(DG)}).

read_template(File) ->
    file:read_file(template_file(File)).

template_file(File) ->
    filename:join([code:priv_dir(edump),
                   "templates", File]).

pids(DG) ->
    [ digraph:vertex(DG, V) || V <- digraph:vertices(DG)].

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
    Info = edump_seg:parse(Seg, Handle),
    add_proc(Id, Info, DG),
    add_links(Id,
              edump_proc:related_procs(Info),
              DG),
    build_graph(Segs, DG, Handle).

add_proc(Id, Info, DG) ->
    digraph:add_vertex(DG, Id, Info).

add_links(Id, Links, DG) ->
    [ add_link(Id, Link, DG) || Link <- Links ],
    ok.

add_link(Self, {link, Who}, DG) ->
    maybe_add_vertex(Who, DG),
    digraph:add_edge(DG, Self, Who, link);
add_link(Self, {spawned_by, Who}, DG) ->
    maybe_add_vertex(Who, DG),
    digraph:add_edge(DG, Self, Who, spawned_by);
add_link(Self, {monitored_by, Who, Ref}, DG) ->
    maybe_add_vertex(Who, DG),
    digraph:add_edge(DG, Who, Self, {monitors, Ref});
add_link(Self, {monitoring, Who, Ref}, DG) ->
    maybe_add_vertex(Who, DG),
    digraph:add_edge(DG, Self, Who, {monitors, Ref}).

maybe_add_vertex(Who, DG) ->
    case digraph:vertex(DG, Who) of
        false ->
            digraph:add_vertex(DG, Who, []);
        _ ->
            ok
    end.
