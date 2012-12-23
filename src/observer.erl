%%%-------------------------------------------------------------------
%%% @author  nisbus <nisbus@gmail.com>
%%% @copyright nisbus (C) 2012, 
%%% @doc
%%%    Create an observer to observe a stream of data.  
%%%    Subscribers can then subscribe to the stream providing the
%%%    observer with a list of filters, an aggregate function and/or
%%%    a window definition.
%%% @end
%%% Created : 21 Dec 2012 by nisbus <nisbus@gmail.com>
%%%-------------------------------------------------------------------
-module(observer).

-behaviour(gen_server).

-include("../include/types.hrl").
%% API
-export([start_link/0, start_link/1,stop/0,stop/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-export([on_next/1, on_error/1, on_completed/0, list_subscribers/0, subscribe/1,unsubscribe/1]).
-export([on_next/2, on_error/2, on_completed/1, list_subscribers/1, subscribe/2,unsubscribe/2]).

-define(SERVER, ?MODULE). 

-record(state, {observers = []}).

%%%===================================================================
%%% API
%%%===================================================================

%%%@doc
%%%  Starts a new observer, only use this if you are using a single observer.
%%%@end
-spec start_link() -> {ok, pid()}.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%@doc
%%%  Starts a new observer with a specific name.
%%%@end
-spec start_link(Name :: atom()) -> {ok,pid()}.
start_link(Name) ->
    gen_server:start_link({local, Name}, ?MODULE, [], []).

%%%@doc
%%%  Stops the observer, if created using start_link().
%%%@end
-spec stop() -> noreply.
stop() ->
    gen_server:cast(?SERVER,stop).

%%%@doc
%%%  Stops an observer by pid.
%%%@end
-spec stop(Pid :: pid()) -> noreply.
stop(Pid) ->
    gen_server:cast(Pid,stop).

%%%@doc
%%%  Subscribes to an observer.
%%%   #subscription{}:
%%%      filters = list of filters to apply to the stream.
%%%@end
-spec subscribe(Subscription :: #subscription{}) -> ok.
subscribe(Subscription) ->
    subscribe(Subscription,?SERVER).

%%%@doc
%%%  Unsubscribes from an observer, takes in the pid of the subscriber.
%%%@end
-spec unsubscribe(SubscriberPid :: pid()) -> ok.
unsubscribe(SubscriberPid) ->
    unsubscribe(SubscriberPid, ?SERVER).

%%%@doc
%%%  Used by the provider to add values to the observer.
%%%@end
-spec on_next(Value :: any()) -> noreply.
on_next(Value) ->
    on_next(Value,?SERVER).

%%%@doc
%%%  Used by the provider to signal an exception.
%%%@end
-spec on_error(Exception :: any()) -> noreply.
on_error(Exception) ->
    on_error(Exception, ?SERVER).

%%%@doc
%%%  Used by the provider to signal the end of stream.
%%%@end
-spec on_completed() -> noreply.
on_completed() ->
    on_completed(?SERVER).


%%%@doc
%%%  Returns a list of subscribers.
%%%@end
-spec list_subscribers() -> [#observer_state{}].
list_subscribers() ->
    list_subscribers(?SERVER).

%%%@doc
%%%  Subscribes to an observer by pid.
%%%   #subscription{}:
%%%      filters = list of filters to apply to the stream.
%%%@end
-spec subscribe(Subscription :: #subscription{}, Pid :: pid()) -> ok.
subscribe(Subscription, Pid) ->
    gen_server:call(Pid,{subscribe, Subscription}).

%%%@doc
%%%  Unsubscribes a subscriber from the observer.
%%%  Takes the pid of the subscriber and the pid of
%%%  of the server.
%%%@end
-spec unsubscribe(SubscriberPid :: pid(), Pid :: pid()) -> ok.
unsubscribe(SubscriberPid,Pid) ->
    gen_server:call(Pid,{unsubscribe, SubscriberPid}).

%%%@doc
%%%  Used by the provider to add values to the observer.
%%%  Pid is the pid of the observer.
%%%@end
-spec on_next(Value :: any(), Pid :: pid()) -> noreply.
on_next(Value,Pid) ->
    gen_server:cast(Pid,{next,Value}).

%%%@doc
%%%  Used by the provider to signal an exception.
%%%  Pid is the pid of the observer.
%%%@end
-spec on_error(Exception :: any(), Pid :: pid()) -> noreply.
on_error(Exception,Pid) ->
    gen_server:cast(Pid,{error,Exception}).

%%%@doc
%%%  Used by the provider to signal the end of stream.
%%%  Pid is the pid of the observer.
%%%@end
-spec on_completed(Pid :: pid()) -> noreply.
on_completed(Pid) ->
    gen_server:cast(Pid,{completed}).

%%%@doc
%%%  Returns a list of currently subscribed subscribers.
%%%  Pid is the pid of the observer.
%%%@end
-spec list_subscribers(Pid :: pid()) -> [#observer_state{}].
list_subscribers(Pid) ->
    gen_server:call(Pid, list).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
%%%@hidden
init([]) ->
    {ok, #state{}}.

%%%@hidden
handle_call(list, _From, #state{observers = Obs}=State) ->
    {reply, Obs, State};

%%%@hidden
handle_call({subscribe, #subscription{filters = F, aggregate = A,
			 window = W}},From,State) ->
    io:format("Creating filters\n"),
    Filters = case io_lib:printable_list(F) of
		  true ->
		      eval:make_fun(F);
		  false ->
		      eval:make_funs(F)
	      end,
    Agg = create_aggregator(A),  
    Window = create_window(W),
    {FromPid, _FromRef} = From,
    io:format("Adding subscriber\n"),
    {reply,ok,add(#observer_state{filters = Filters,
				  aggregate = Agg,
				  window = Window,
				  notify = FromPid},
		  State)};	   

%%%@hidden
handle_call({unsubscribe, Pid},_From,State) ->  
    io:format("unsubscribing\n"),
    {reply,ok,remove(Pid,State)};	   

%%%@hidden
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%%@hidden
handle_cast(stop,State) ->    
    {stop,normal,State};	   

%%%@hidden
handle_cast({next,Value}, #state{observers = Obs} = State) ->
    lists:foreach(fun(#observer_state{filters = F, aggregate =_A,
			 window = _W, events = _E,
			 notify = _N}) ->
			  case run_filters(F,Value) of
			      true ->
				  io:format("Value passed filters ~p\n",[Value]),
				  case run_aggregate(_A,Value) of
				      {true, Aggregate} ->
					  io:format("Value aggregated ~p\n",[Aggregate]),
					  case run_window(_W,Aggregate) of
					      {true, Window} ->
						  io:format("Value not windowed ~p\n",[Window]),
						  io:format("Sending value to subscriber ~p\n",[_N]),
						  _N ! {Window, self()};
					      _ ->
						  io:format("value is windowed, window will notify subscriber\n"),
						  void
					  end;
				      false ->
					  void
				  end;
			      false ->
				  io:format("Value received the got filtered away\n"),
				  void
			  end			  			      
		  end,Obs),
    {noreply, State};

%%%@hidden
handle_cast({error,Exception}, #state{observers = Obs} = State) ->
    lists:foreach(fun(O) ->
			  O ! {error,Exception, self()}
		  end,Obs),
    {noreply, State};

%%%@hidden
handle_cast(completed, #state{observers = Obs} = State) ->
    lists:foreach(fun(O) ->			  
			  O ! {completed, self()}
		  end,Obs),
    {noreply, State};

%%%@hidden
handle_cast(_Msg, State) ->
    io:format("Unknown message ~p~n",[_Msg]),
    {noreply, State}.    

%%%@hidden
handle_info(_Info, State) ->
    {noreply, State}.

%%%@hidden
terminate(_Reason, #state{observers = Obs} = _State) ->
    lists:foreach(fun(O) ->
			  #observer_state{aggregate = A, window = W} = O,
			  case is_pid(A) of
			      true ->
				  aggregate:stop(A);
			      false -> void
			  end,
			  case is_pid(W) of
			      true -> window:stop(W);
			      _ -> void
			  end
		  end,Obs).

%%%@hidden
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
%%%@hidden
remove(Pid,#state{observers = Obs} =State) ->
    Subscriber = lists:keyfind(Pid, #observer_state.notify,Obs),

    io:format("Subscriber ~p\n",[Subscriber]),
    case Subscriber of
	[] -> State;
	_ ->
	    Window = Subscriber#observer_state.window,
	    Aggregate = Subscriber#observer_state.aggregate,
	    stop_window(Window),
	    stop_aggregate(Aggregate),
	    State#state{observers= lists:delete(Subscriber,Obs)}
    end.

%%%@hidden
add(Observer,#state{observers = Obs}) ->    
    #state{observers= Obs++[Observer]}.

%%%@hidden
run_filters([], _Value) ->
    true;
run_filters(undefined,_Value) ->
    true;
run_filters(F,Value) when is_function(F) ->
    filter:match([F],Value);
run_filters(F,Value) ->
    filter:match(F,Value).

%%%@hidden
run_aggregate([],Value) ->
    {true, Value};
run_aggregate(undefined,Value) ->
    {true,Value};
run_aggregate(A,Value) when is_pid(A) ->
    Aggregated = aggregate:add(A,Value),
    {true, Aggregated};
run_aggregate(_A, _Value) ->
    io:format("Invalid aggregate ~p, ~p\n",[_A, _Value]),
    false.
    
%%%@hidden
run_window({timed,W},Value) when is_pid(W) ->
    timed_window:update(Value,W),
    {false,Value};
run_window({sized,W},Value) when is_pid(W) ->
    sized_window:update(W,Value),
    {false,Value};
run_window(_,Value) ->    
    {true,Value}.

%%%@hidden
create_aggregator([]) ->
    undefined;
create_aggregator(undefined) ->
    undefined;
create_aggregator(A) when is_function(A) ->
    {ok, Pid} = aggregate:start_link(A),
    Pid;
create_aggregator(A) when is_list(A) ->
    AAsFun = case io_lib:printable_list(A) of
		 true ->
		     eval:make_fun(A);
		 false ->
		     eval:make_funs(A)
	     end,
    {ok,Pid} = aggregate:start_link(AAsFun),
    Pid.

%%%@hidden
create_window(undefined) ->
    undefined;
create_window({timed, {H,M,S},Precision, Listener}) ->    
    {ok, Pid} = timed_window:start_link({H,M,S},Precision, Listener),
    {timed,Pid};    
create_window({sized, Size, Listener}) ->    
    {ok, Pid} = sized_window:start_link(Size,Listener),
    {sized,Pid}.
    
%%%@hidden
stop_window({sized,W}) when is_pid(W)->
    sized_window:stop(W);
stop_window({timed,W}) when is_pid(W)->
    timed_window:stop(W);
stop_window(_) ->
    ok.

%%%@hidden
stop_aggregate(A) when is_pid(A) ->
    aggregate:stop(A);
stop_aggregate(_) ->
    ok.
