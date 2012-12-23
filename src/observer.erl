%%%-------------------------------------------------------------------
%%% @author  nisbus <nisbus@gmail.com>
%%% @copyright (C) 2012, 
%%% @doc
%%%
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

-export([on_next/1, on_error/1, on_completed/0, list_observers/0, subscribe/1,unsubscribe/1]).
-export([on_next/2, on_error/2, on_completed/1, list_observers/1, subscribe/2,unsubscribe/2]).

-define(SERVER, ?MODULE). 

-record(state, {observers = []}).

%%%===================================================================
%%% API
%%%===================================================================
subscribe(Subscription) ->
    gen_server:call(?SERVER,{subscribe, Subscription}).
unsubscribe(P) ->
    gen_server:call(?SERVER,{unsubscribe, P}).
on_next(Value) ->
    gen_server:cast(?SERVER,{next,Value}).
on_error(Exception) ->
    gen_server:cast(?SERVER,{error,Exception}).
on_completed() ->
    gen_server:cast(?SERVER,{completed}).
list_observers() ->
    gen_server:call(?SERVER, list).

subscribe(Subscription, Name) ->
    gen_server:call(Name,{subscribe, Subscription}).
unsubscribe(P,Name) ->
    gen_server:call(Name,{unsubscribe, P}).
on_next(Value,Name) ->
    gen_server:cast(Name,{next,Value}).
on_error(Exception,Name) ->
    gen_server:cast(Name,{error,Exception}).
on_completed(Name) ->
    gen_server:cast(Name,{completed}).
list_observers(Name) ->
    gen_server:call(Name, list).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

start_link(Name) ->
    gen_server:start_link({local, Name}, ?MODULE, [], []).

stop() ->
    gen_server:cast(?SERVER,stop).
stop(Name) ->
    gen_server:cast(Name,stop).
%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    {ok, #state{}}.

handle_call(list, _From, #state{observers = Obs}=State) ->
    {reply, Obs, State};

handle_call({subscribe, #observer_state{filters = F, aggregate = A,
			 window = W}},From,State) ->
    Filters = case io_lib:printable_list(F) of
		  true ->
		      eval:make_fun(F);
		  false ->
		      eval:make_funs(F)
	      end,
    Agg = create_aggregator(A),  
    Window = create_window(W),
    {FromPid, _FromRef} = From,
    {reply,ok,add(#observer_state{filters = Filters,
				  aggregate = Agg,
				  window = Window,
				  notify = FromPid},
		  State)};	   

handle_call({unsubscribe, Pid},_From,State) ->  
    io:format("unsubscribing"),
    {reply,ok,remove(Pid,State)};	   

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(stop,State) ->    
    {stop,normal,State};	   

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
				  void
			  end			  			      
		  end,Obs),
    {noreply, State};

handle_cast({error,Exception}, #state{observers = Obs} = State) ->
    lists:foreach(fun(O) ->
			  O ! {error,Exception, self()}
		  end,Obs),
    {noreply, State};

handle_cast(completed, #state{observers = Obs} = State) ->
    lists:foreach(fun(O) ->			  
			  O ! {completed, self()}
		  end,Obs),
    {noreply, State};

handle_cast(_Msg, State) ->
    io:format("Unknown message ~p~n",[_Msg]),
    {noreply, State}.    

handle_info(_Info, State) ->
    {noreply, State}.

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

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
remove(Pid,#state{observers = Obs} =State) ->
    Subscriber = lists:keyfind(Pid, #observer_state.notify,Obs),
    io:format("Subscriber ~p\n",[Subscriber]),
    case Subscriber of
	[] -> State;
	_ -> State#state{observers= lists:delete(Subscriber,Obs)}
    end.

add(Observer,#state{observers = Obs}) ->    
    #state{observers= Obs++[Observer]}.

run_filters([], _Value) ->
    true;
run_filters(undefined,_Value) ->
    true;
run_filters(F,Value) when is_function(F) ->
    filter:match([F],Value);
run_filters(F,Value) ->
    filter:match(F,Value).

run_aggregate([],Value) ->
    {true, Value};
run_aggregate(undefined,Value) ->
    {true,Value};
run_aggregate(A,Value) when is_pid(A) ->
    Aggregated = aggregate:add(A,Value),
    {true, Aggregated};
run_aggregate(_A, _Value) ->
    false.
    
run_window({timed,W},Value) when is_pid(W) ->
    timed_window:update(Value,W),
    {false,Value};
run_window({sized,W},Value) when is_pid(W) ->
    sized_window:update(W,Value),
    {false,Value};
run_window(_,Value) ->    
    {true,Value}.


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

create_window(undefined) ->
    undefined;
create_window({timed, {H,M,S},Precision, Listener}) ->    
    {ok, Pid} = timed_window:start_link({H,M,S},Precision, Listener),
    {timed,Pid};    
create_window({sized, Size, Listener}) ->    
    {ok, Pid} = sized_window:start_link(Size,Listener),
    {sized,Pid}.
    
