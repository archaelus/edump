# edump

Efficient Erlang Crashdump Analysis Tools

## Build

    $ rebar3 compile
    $ rebar3 rdtl
    $ rebar3 escriptize

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

This produces a graph like this when rendered with GraphViz (edump produces only the `.dot` file):
![image](test/data/erl_crash.png)

## Edump escript

The `edump` escript provides a commandline interface to some common functionality:

* `edump index` parses dump files and produces index files

```
$ edump index -c full test/data/erl_crash.dump                                  
Indexing "test/data/erl_crash.dump" [{checking,full},
                                     {file,"test/data/erl_crash.dump"},
                                     {rebuild,false}]
Indexed "test/data/erl_crash.dump" in 0.218382s
```

* `edump graph` produces a GraphViz dot file from a dump file. It will  index the dump file if necessary.

```
edump graph test/data/erl_crash.dump -d test/data/erl_crash.full.dot -g "rankdir="TB";size=\"12,8\""
Graph opts: #{graph_attributes => ["rankdir=TB","size=\"12,8\""],
              include_edge => #Fun<edump_proc_dot.fewer_edges.3>}
Wrote graph to "test/data/erl_crash.full.dot"
```

* `edump info` prints a description of various information from the dump file. Without options (or `--info basic`), edump presents a basic summary:

```
$ edump info test/data/erl_crash.dump
Crashdump "test/data/erl_crash.dump"
Crashed: <<"Wed Oct  3 23:53:30 2012">>
Slogan: A test crash
Total memory:  9.060 Mb
   78.56% system:  7.117 Mb
   44.69% code:  4.049 Mb
   21.44% processes:  1.943 Mb (100.00% used)
    7.10% ets:  0.644 Mb
    2.14% atom:  0.194 Mb ( 90.07% used)
    1.86% binary:  0.168 Mb
```

* `edump info --info processes` (a really clumsy CLI, I don't know how to fix that yet)

```
$ edump info test/data/erl_crash.dump --info processes                                  
Crashdump "test/data/erl_crash.dump"
Processes (32 of 32):
   1         <0.31.0>                      (Running, 0 msgq, 4181 mem, 86159 reds)
   2          <0.6.0> application_controll (Waiting, 0 msgq, 28657 mem, 7881 reds)
   3          <0.3.0>      erl_prim_loader (Waiting, 0 msgq, 6765 mem, 270453 reds)
   4         <0.10.0>           kernel_sup (Waiting, 0 msgq, 4181 mem, 37621 reds)
   5         <0.18.0>          code_server (Waiting, 0 msgq, 4181 mem, 155017 reds)
   6          <0.0.0>                 init (Waiting, 0 msgq, 2584 mem, 3414 reds)
   7         <0.24.0>                      (Waiting, 0 msgq, 2584 mem, 24580 reds)
   8         <0.25.0>                      (Waiting, 0 msgq, 2584 mem, 6347 reds)
   9         <0.22.0>             user_drv (Waiting, 0 msgq, 987 mem, 8140 reds)
  10          <0.5.0>         error_logger (Waiting, 0 msgq, 610 mem, 280 reds)
  11         <0.41.0>                      (Waiting, 0 msgq, 610 mem, 957 reds)
  12         <0.44.0>         disk_log_sup (Waiting, 0 msgq, 610 mem, 236 reds)
  13          <0.8.0>                      (Waiting, 0 msgq, 377 mem, 44 reds)
  14         <0.17.0>        file_server_2 (Waiting, 0 msgq, 377 mem, 514 reds)
  15         <0.27.0>      kernel_safe_sup (Waiting, 0 msgq, 377 mem, 195 reds)
  16         <0.45.0>      disk_log_server (Waiting, 0 msgq, 377 mem, 228 reds)
  17          <0.9.0>                      (Waiting, 0 msgq, 233 mem, 69 reds)
  18         <0.11.0>                  rex (Waiting, 0 msgq, 233 mem, 35 reds)
  19         <0.12.0>   global_name_server (Waiting, 0 msgq, 233 mem, 50 reds)
  20         <0.13.0>                      (Waiting, 0 msgq, 233 mem, 20 reds)
  21         <0.14.0>                      (Waiting, 0 msgq, 233 mem, 3 reds)
  22         <0.15.0>              inet_db (Waiting, 0 msgq, 233 mem, 234 reds)
  23         <0.16.0>         global_group (Waiting, 0 msgq, 233 mem, 59 reds)
  24         <0.19.0>   standard_error_sup (Waiting, 0 msgq, 233 mem, 41 reds)
  25         <0.20.0>       standard_error (Waiting, 0 msgq, 233 mem, 9 reds)
  26         <0.21.0>                      (Waiting, 0 msgq, 233 mem, 62 reds)
  27         <0.23.0>                 user (Waiting, 0 msgq, 233 mem, 36 reds)
  28         <0.26.0>                      (Waiting, 0 msgq, 233 mem, 268 reds)
  29         <0.34.0>                      (Waiting, 0 msgq, 233 mem, 23 reds)
  30         <0.35.0>                      (Waiting, 0 msgq, 233 mem, 49 reds)
  31         <0.36.0>            edump_sup (Waiting, 0 msgq, 233 mem, 102 reds)
  32         <0.37.0>         edump_viewer (Waiting, 0 msgq, 233 mem, 27 reds)
```