-type segment_id () :: {mod, atom()} | {atom(), any()} | atom().

-record(seg, {id :: segment_id(),
              seg_start = unknown :: pos_integer() | unknown,
              data_start = unknown :: pos_integer() | undefined | unknown,
              seg_end = unknown :: pos_integer() | unknown
             }).
