# edump

Efficient Erlang Crashdump Analysis Tools

## Build

    $ rebar3 compile

## Use

### Reading a crashdump

```
$ rebar3 shell
> Handle = edump:open("/path/to/erl_crash.dump").
...<edump Handle>
```

`edump:open` parses a crashdump file, creates an index of segments in the dump, (by default) writes the index file to `CrashdumpFile ++ ".eidx"` and returns a usable handle to the index. Further crashdump investigation functions use a `Handle`.

### Basic info

```
> edump:ports(Handle).
[{port,<<"#Port<0.1>">>},
 {port,<<"#Port<0.47>">>},
 {port,<<"#Port<0.404>">>},
 {port,<<"#Port<0.413>">>}]
> edump:processes(Handle).
[{proc,<<"<0.0.0>">>},
 {proc,<<"<0.3.0>">>},
 {proc,<<"<0.5.0>">>},
 {proc,<<"<0.6.0>">>},
 {proc,<<"<0.8.0>">>},
 {proc,<<"<0.9.0>">>},
 {proc,<<"<0.10.0>">>}|...]
```

Indexes store data about crashdump file segments. Segments have ids, for instance `{proc, <<"<0.0.0>">>}` or `{port, <<"#Port<0.1>">>}`. You can get more information about a segment by id:

```
> edump:info({port,<<"#Port<0.1>">>}, Handle).
{port,[{slot,1},
       {connected,{proc,<<"<0.3.0>">>}},
       {links,{proc,<<"<0.3.0>">>}},
       {driver,<<"efile">>}],
      []}
```

### Drawing Process Graphs

There are some analysis modules that will construct a graph of erlang entities (port, processes, nodes, etc) from the data in a crashdump and turn these into GraphViz dot files for viewing.

```
> Graph = edump:proc_graph(Handle)
{digraph,700451,704546,708644,true}
> edump_proc_dot:to_dot("priv/erl_crash.dot", Graph,
                        #{include_edge => fun edump_proc_dot:fewer_edges/3}).
ok
```

This produces a graph like this:
![image](priv/erl_crash.png)