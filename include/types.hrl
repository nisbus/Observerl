%%% *-* erlang *-*
-type timestamp() :: {integer(),integer(),integer()}.
-type timeframe() :: {integer(), integer(), integer()}.
-type timestamped() :: ({any(),timestamp()}).
-type window_spec() :: {timed, timeframe(), fun(), undefined|fun(), undefined|any(), integer()} |
{sized, integer(), fun(), undefined|fun(), undefined|any()}.

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
