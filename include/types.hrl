%%% *-* mode: erlang *-*

%%%@doc
%%%  used internally by the timed_window to timestamp values as they are added.
%%%@end
-type timestamp() :: {integer(),integer(),integer()}.
%%%@doc
%%%  used to specify the size of a window, {hour, minute, second, millisecond (optional)}.
%%%@end
-type timeframe() :: {integer(), integer(), integer(), integer() | undefined}.
-type timestamped() :: ({any(),timestamp()}).

%%%@doc
%%% Used to define a window to apply to the stream.
%%% there are two types of windows available:
%%%    timed : given a #timeframe{}, precision (ms), and a pid to notify
%%%            when the time interval passes.
%%%            Adds to the window on on_next from the observer and removes
%%%            items from the window when the items time is older than the
%%%            specified timeframe.
%%%    sized : given a size this window will not notify the subscriber until
%%%            the window contains at least the number of items specified by size
%%%            After the size has been reached, the window will notify the subscriber
%%%            with the contents of the window on each added item.
-type window_spec() :: {timed, timeframe(), integer(), pid()} |
		       {sized, integer(), pid()}.

%%%@doc
%%%  Gets handed to the observer using the subscribe command.
%%%     filters: the subscriber can provide a list of functions to filter the stream by.
%%%              all the functions must be of the form fun(X) -> true | false end.
%%%              To get a value published to the subscriber, all the filters must return true.
%%%     aggregate: a function to aggregate the values from the stream, this function must take the
%%%                form fun(X,Y) -> any() end. where X is the incoming value and Y is the current
%%%                aggregated value.
%%%     window : see the #window_spec{}
%%%@end
-record(subscription,
	{
	  filters :: undefined | [fun()] | [string()] | [binary()], 
transform :: undefined | fun() | string() | binary(),
aggregate :: undefined | fun() | string() | binary() ,
window :: undefined | window_spec()
}).

-record(observer_state, 
	{
	  filters :: undefined | [fun()],
aggregate :: undefined | pid(),
window :: undefined | pid(),
transform :: undefined | fun(),
events :: [any()],
notify :: pid()
}).
