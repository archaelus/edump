-record(seg, {id :: binary(),
              seg_start :: pos_integer(),
              data_start = unknown :: pos_integer() | undefined | unknown,
              seg_end = unknown :: pos_integer() | unknown}).
