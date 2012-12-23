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
-export([start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-export([add/1, current/0]).
-define(SERVER, ?MODULE). 

-record(state, {aggregated :: any(),
		aggregator_fun :: fun()
	       }).

%%%===================================================================
%%% API
%%%===================================================================
add(Value) ->
    gen_server:cast(?MODULE, {add, Value}).
current() ->
    gen_server:call(?MODULE, current).
%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(Aggregator) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [Aggregator], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([Aggregator]) ->
    {ok, #state{aggregator_fun = Aggregator}}.

handle_call(current, _From, #state{aggregated = Result} = State) ->
    {reply, Result, State};

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast({add,Value}, #state{aggregator_fun = F, aggregated = SoFar} = State) ->    
    {noreply, State#state{aggregated = F(Value,SoFar)}};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
