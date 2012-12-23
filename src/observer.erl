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
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-export([on_next/1, on_error/1, on_completed/0, add/1, remove/1, list_observers/0, subscribe/1]).

-define(SERVER, ?MODULE). 

-record(state, {observers = []}).

%%%===================================================================
%%% API
%%%===================================================================
subscribe(Observer) ->
    gen_server:cast(?SERVER,{subscribe, Observer}).

on_next(Value) ->
    gen_server:cast(?SERVER,{next,Value}).
on_error(Exception) ->
    gen_server:cast(?SERVER,{error,Exception}).
on_completed() ->
    gen_server:cast(?SERVER,{completed}).
add(Observer) ->
    gen_server:cast(?SERVER, {add, Observer}).
remove(Observer) ->
    gen_server:cast(?SERVER, {remove, Observer}).
list_observers() ->
    gen_server:call(?SERVER, list).
%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    {ok, #state{}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call(list, _From, #state{observers = Obs}=State) ->
    {reply, Obs, State};

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast({subscribe, #observer_state{filters = F, aggregate =A,
			 window = _W, events = _E,
			 notify = _N} = Observer},State) ->
    Filters = case io_lib:printable_list(F) of
		  true ->
		      eval:make_fun(F);
		  false ->
		      eval:make_funs(F)
	      end,
    Agg = eval:make_fun(A),    
    {noreply,add(Observer#observer_state{filters = Filters,
					 aggregate = Agg},State)};	   

handle_cast({next,Value}, #state{observers = Obs} = State) ->
    lists:foreach(fun(#observer_state{filters = _F, aggregate =_A,
			 window = _W, events = _E,
			 notify = _N}) ->
			  
			  _N ! {next,Value, self()}
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

handle_cast({add, Observer}, State) ->
    {noreply, add(Observer,State)};

handle_cast({remove,Observer}, State) ->
    {noreply, remove(Observer,State)};

handle_cast(_Msg, State) ->
    io:format("Unknown message ~p~n",[_Msg]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
remove(Observer,#state{observers = Obs}) ->
    NewObs = lists:delete(Observer,Obs),
    #state{observers= NewObs}.

add(Observer,#state{observers = Obs}) ->    
    #state{observers= Obs++[Observer]}.
