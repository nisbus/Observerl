%%%-------------------------------------------------------------------
%%% @author nisbus <nisbus@gmail.com>
%%% @copyright (C) 2012, nisbus
%%% @doc
%%%   A process for storing a window of a timed size.
%%%   The window monitors the window elements at the time in ms given 
%%%   by the precision parameter.
%%%   The window will also notify subscribers with the contents of the 
%%%   window at the interval given by the precision parameter.
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
%%% @doc
%%%  Updates the current window with new data
%%%  Instance is the pid of the window.
%%% @end
-spec update(Data :: any(), Instance :: pid()) -> noreply.
update(Data,Instance) ->
    gen_server:cast(Instance,{incoming_data,Data}).

%%% @doc 
%%%   Returns the data in the current window.
%%%   Instance is the pid of the window
%%% @end
-spec current_window(Instance :: pid()) -> [any()].
current_window(Instance) ->
    gen_server:call(Instance,current_window).	

%%% @doc
%%%   Stops the window process
%%%   Instance is the pid of the window
%%% @end
-spec stop(Instance :: pid()) -> noreply.
stop(Instance) ->
    gen_server:cast(Instance,stop).

%%%--------------------------------------------------------------------
%%% @doc
%%% Starts the server.
%%%   WinSize is the timeframe to keep windowed.
%%%   Precision is the time in ms at which the window will expire items
%%%   and notify the subscriber.
%%%   ListenerPid is the pid of the subscriber
%%% @end
%%%--------------------------------------------------------------------
-spec start_link(WinSize :: timeframe(), Precision :: integer(), Listener :: pid()) -> {ok,pid()}| ignore | {error,Error :: any()}.
start_link(WinSize, Precision, Listener) ->
    Ref = erlang:make_ref(),
    Id = list_to_atom(erlang:ref_to_list(Ref)),
    gen_server:start_link({local, Id}, ?MODULE, [{WinSize, Precision, Listener}], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%% @hidden
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

%%% @hidden
handle_call(current_window, _From, #state{current_window = Window} = State) ->
    {reply, Window, State};

%%% @hidden
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%% @hidden
handle_cast(stop, State) ->
    {stop,normal, State};

%%% @hidden
handle_cast({incoming_data, Data},#state{current_window = W} = State) ->
    NewWindow = add_new(W, Data),
    {noreply,State#state{current_window = NewWindow}};

%%% @hidden
handle_cast(_Msg, State) ->
    {noreply, State}.

%%% @hidden
handle_info(check_expired, #state{current_window = W, window_length_in_ms = MS, listener = Pid} = State) ->
    NewWin = process_expired(W,MS),
    io:format("sending window on time ~p\n",[NewWin]),
    Pid ! {window, NewWin, self()},
    {noreply, State#state{current_window = NewWin}};

%%% @hidden
handle_info(_Info, State) ->
    {noreply, State}.

%%% @hidden
terminate(_Reason, #state{check_expired_timer = T} = _State) ->
    case T of
    	undefined ->
    	    void;
    	_ ->
	    io:format("timer cancelled\n"),
    	    timer:cancel(T)
    end,
    ok.

%%% @hidden
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%%Internal functions
%%%===================================================================

%%% @hidden
get_old_data(Data, WinSize) ->
    CurrentTime = now_to_milliseconds(erlang:now()),    
    Expired = CurrentTime-WinSize,
    [{Time,X} || {Time,X} <- Data, Time < Expired].

%%% @hidden
process_expired(Window, MS) ->
    ExpiredData = get_old_data(Window,MS),
    case ExpiredData of
	[] ->
	    Window;
	_ ->
	    lists:subtract(Window,ExpiredData)
    end.

%%% @hidden
add_new(Window,NewData) ->
    Add = {now_to_milliseconds(erlang:now()),NewData},
    lists:append(Window,[Add]).
    
%%% @hidden
timeframe_to_milliseconds({Hour, Minute,Sec}) ->
    timeframe_to_milliseconds({Hour,Minute,Sec,0});
timeframe_to_milliseconds({Hour, Minute,Sec,Milliseconds}) ->
    (Hour*3600000)+(Minute*60000)+(Sec*1000)+Milliseconds.
    
%%
%%   Thanks to zaphar for this gist
%%   https://gist.github.com/104903
%%
%%% @hidden
now_to_seconds({Mega, Sec, _}) ->
    (Mega * 1000000) + Sec.   
    
now_to_milliseconds({Mega, Sec, Micro}) ->
    now_to_seconds({Mega, Sec, Micro}) * 1000.
