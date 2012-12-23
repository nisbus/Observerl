%%%-------------------------------------------------------------------
%%% @author  nisbus <nisbus@gmail.com>
%%% @copyright nisbus (C) 2012, 
%%% @doc
%%%    Gets started when an observer gets a subscription containing
%%%    an aggregate function.
%%%    Each time the observer gets a new value it will call the 
%%%    aggregator which returns the aggregated value.
%%% @end
%%% Created : 21 Dec 2012 by  nisbus <nisbus@gmail.com>
%%%-------------------------------------------------------------------
-module(aggregate).

-behaviour(gen_server).

%% API
-export([start_link/1, stop/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-export([add/2, current/1]).
-define(SERVER, ?MODULE). 

-record(state, {aggregated :: any(),
		aggregator_fun :: fun()
	       }).

%%%===================================================================
%%% API
%%%===================================================================
%%%@doc
%%% Adds a new value to the aggregate and returns the aggregated value.
%%%@end
-spec add(Instance :: pid(), Value :: any()) -> any().
add(Instance,Value) ->
    gen_server:call(Instance, {add, Value}).

%%%@doc
%%% Returns the current value of the aggregate.
%%%@end
-spec current(Instance :: pid()) -> any().
current(Instance) ->
    gen_server:call(Instance, current).

%%%@doc
%%% Stops the aggregating process.
%%%@end
-spec stop(Instance :: pid()) -> ok.
stop(Instance) ->
    gen_server:cast(Instance,stop).

%%%@doc
%%% Starts a new aggregator for a subscriber.
%%%@end
-spec start_link(AggregatorFun :: string()| fun()) -> {ok,pid()}.
start_link(AggregatorFun) ->
    Ref = erlang:make_ref(),
    Id = list_to_atom(erlang:ref_to_list(Ref)),
    gen_server:start_link({local, Id}, ?MODULE, [AggregatorFun], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%%@hidden
init([Aggregator]) ->
    {ok, #state{aggregator_fun = Aggregator}}.

%%%@hidden
handle_call({add,Value},_From, #state{aggregator_fun = F, aggregated = SoFar} = State) ->    
    Aggregated = case SoFar of
		     undefined -> Value;
		     _ -> F(Value,SoFar)
		 end,
    {reply, Aggregated, State#state{aggregated = Aggregated}};

%%%@hidden
handle_call(current, _From, #state{aggregated = Result} = State) ->
    {reply, Result, State};

%%%@hidden
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%%@hidden
handle_cast(stop, State) ->
    {stop,normal, State};

%%%@hidden
handle_cast(_Msg, State) ->
    {noreply, State}.

%%%@hidden
handle_info(_Info, State) ->
    {noreply, State}.

%%%@hidden
terminate(_Reason, _State) ->
    ok.

%%%@hidden
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
