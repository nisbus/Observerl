%%%-------------------------------------------------------------------
%%% @author nisbus <nisbus@gmail.com>
%%% @copyright (C) 2012, nisbus
%%% @doc
%%%   winderl is for managing windowed streams of data.
%%%   It provides an API for adding data to the window and getting notifications 
%%%   when data expires from the window.
%%%   It also provides calls to get the current window and the current external state.
%%%
%%%   When starting the server you need to give it an update fun to execute on incoming data.
%%%   You can optionally provide it with an expire fun to execute on all expired data.
%%%   The server can also manage state given to you in the start but if you do so you must also
%%%   provide update- and (optionally) expired funs that takes in two arguments to update the external state
%%%   when data arrives or is expired.
%%% @end
%%% Created : 18 Nov 2012 by nisbus <nisbus@gmail.com>
%%%-------------------------------------------------------------------
-module(timed_window).

-behaviour(gen_server).

-include("../include/types.hrl").
%% API
-export([start_link/3, update/2,current_window/1, stop/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 



-record(state, 
	{
	  window_length :: timeframe(),
	  current_window :: [],
	  window_length_in_ms :: integer(),
	  check_expired_timer :: reference(),
	  listener :: pid()
	}).

%%%===================================================================
%%% API
%%%===================================================================
%% @doc
%%  Updates the current window with new data
%% @end
update(Data,Instance) ->
    gen_server:cast(Instance,{incoming_data,Data}).

%% @doc Returns the data in the current window
current_window(Instance) ->
    gen_server:call(Instance,current_window).	

stop(Instance) ->
    gen_server:cast(Instance,stop).
%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%--------------------------------------------------------------------
-spec start_link(WinSize :: timeframe(), Precision :: integer(), Listener :: pid()) -> {ok,pid()}| ignore | {error,Error :: any()}.
start_link(WinSize, Precision, Listener) ->
    Ref = erlang:make_ref(),
    Id = list_to_atom(erlang:ref_to_list(Ref)),
    gen_server:start_link({local, Id}, ?MODULE, [{WinSize, Precision, Listener}], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([{WinSize,Precision,Listener}]) ->    
    {ok,{interval,TRef}} = timer:send_interval(1*Precision, check_expired),
    {ok, #state
     {
       window_length=WinSize, 
       window_length_in_ms = timeframe_to_milliseconds(WinSize),
       check_expired_timer = TRef,
       current_window = [],
       listener = Listener}
    }.


handle_call(current_window, _From, #state{current_window = Window} = State) ->
    {reply, Window, State};

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(stop, State) ->
    {stop,normal, State};

handle_cast({incoming_data, Data},#state{current_window = W} = State) ->
    NewWindow = add_new(W, Data),
    {noreply,State#state{current_window = NewWindow}};

handle_cast(_Msg, State) ->
    {noreply, State}.

%% @doc
%%    This gets called internally by the timer every period
%% @end
handle_info(check_expired, #state{current_window = W, window_length_in_ms = MS, listener = Pid} = State) ->
    NewWin = process_expired(W,MS),
    io:format("sending window on time ~p\n",[NewWin]),
    Pid ! {window, NewWin, self()},
    {noreply, State#state{current_window = NewWin}};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, #state{check_expired_timer = T} = _State) ->
    case T of
    	undefined ->
    	    void;
    	_ ->
	    io:format("timer cancelled\n"),
    	    timer:cancel(T)
    end,
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%%Internal functions
%%%===================================================================

%% @doc
%%    Returns old items from the current window
%% @end
get_old_data(Data, WinSize) ->
    CurrentTime = now_to_milliseconds(erlang:now()),    
    Expired = CurrentTime-WinSize,
    [{Time,X} || {Time,X} <- Data, Time < Expired].

%% @doc
%%   Removes each expired element and updates the window.
%% @end
process_expired(Window, MS) ->
    ExpiredData = get_old_data(Window,MS),
    case ExpiredData of
	[] ->
	    Window;
	_ ->
	    lists:subtract(Window,ExpiredData)
    end.

%% @doc
%%    Adds the new data (with timestamp) to the window.
%% @end
add_new(Window,NewData) ->
    Add = {now_to_milliseconds(erlang:now()),NewData},
    lists:append(Window,[Add]).
    
timeframe_to_milliseconds({Hour, Minute,Sec}) ->
    timeframe_to_milliseconds({Hour,Minute,Sec,0});
timeframe_to_milliseconds({Hour, Minute,Sec,Milliseconds}) ->
    (Hour*3600000)+(Minute*60000)+(Sec*1000)+Milliseconds.
    
%% @doc
%%   Thanks to zaphar for this gist
%%   https://gist.github.com/104903
%% @end
now_to_seconds({Mega, Sec, _}) ->
    (Mega * 1000000) + Sec.   
    
now_to_milliseconds({Mega, Sec, Micro}) ->
    now_to_seconds({Mega, Sec, Micro}) * 1000.
