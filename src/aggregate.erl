%%%-------------------------------------------------------------------
%%% @author  nisbus <nisbus@gmail.com>
%%% @copyright nisbus (C) 2012, 
%%% @doc
%%%
%%% @end
%%% Created : 21 Dec 2012 by  nisbus <nisbus@gmail.com>
%%%-------------------------------------------------------------------
-module(aggregate).

-behaviour(gen_server).

%% API
-export([start_link/1, stop/1]).

%% gen_server callbacksnoreply
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
add(Instance,Value) ->
    gen_server:call(Instance, {add, Value}).
current(Instance) ->
    gen_server:call(Instance, current).
stop(Instance) ->
    gen_server:cast(Instance,stop).
%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(AggregatorFun) ->
    Ref = erlang:make_ref(),
    Id = list_to_atom(erlang:ref_to_list(Ref)),
    gen_server:start_link({local, Id}, ?MODULE, [AggregatorFun], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([Aggregator]) ->
    {ok, #state{aggregator_fun = Aggregator}}.

handle_call({add,Value},_From, #state{aggregator_fun = F, aggregated = SoFar} = State) ->    
    Aggregated = case SoFar of
		     undefined -> Value;
		     _ -> F(Value,SoFar)
		 end,
    {reply, Aggregated, State#state{aggregated = Aggregated}};

handle_call(current, _From, #state{aggregated = Result} = State) ->
    {reply, Result, State};

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(stop, State) ->
    {stop,normal, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
