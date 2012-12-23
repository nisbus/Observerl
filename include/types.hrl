%%% *-* erlang *-*
-type timestamp() :: {integer(),integer(),integer()}.
-type timeframe() :: {integer(), integer(), integer()}.
-type timestamped() :: ({any(),timestamp()}).
-type window_spec() :: {timed, timeframe(), integer(), pid()} |
		       {sized, integer(), pid()}.

-record(subscription,
	{
	  filters :: undefined | [fun()] | [string()],
aggregate :: undefined | fun() | string(),
window :: undefined | window_spec()
}).

-record(observer_state, 
	{
	  filters :: undefined | [fun()],
aggregate :: undefined | pid(),
window :: undefined | pid(),
events :: [any()],
notify :: pid()
}).
