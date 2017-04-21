%% Date: 28.12.16 - 18:28
%% Ⓒ 2016 LineMetrics GmbH
-author("Alexander Minichmair").

-include("df_types.hrl").

-ifdef(debug).
-define(LOG(Msg, Args), io:format(Msg ++ "~n", Args)).
-else.
-define(LOG(Msg, Args), ok).
-endif.

-record(data_point, {
   ts                :: non_neg_integer(), %% timestamp in ms
   fields   = []     :: list(),
   tags     = []     :: list(),
   id       = <<>>   :: binary()
}).

-record(data_batch, {
   id                :: binary(),
   points            :: list(#data_point{}),
   start             :: non_neg_integer(),
   ed                :: non_neg_integer()
}).


-record(subscription, {
   flow_mode = push,
   publisher_pid,
   publisher_port,
   subscriber_pid,
   subscriber_port,
   out_buffer,
   pending = false
}).
