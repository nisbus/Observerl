-type timestamp() :: {integer(),integer(),integer()}.
-type timestamped() :: ({any(),timestamp()}).

-record(observer_state, 
	{
	  filters :: [fun()],
aggregate :: pid(),
window :: timespan | integer(),
events :: [any()],
notify :: pid()
}).
