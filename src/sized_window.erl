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
-module(sized_window).

-behaviour(gen_server).

%% API
-export([start_link/2, update/2,current_window/1, external_state/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 


-record(state, 
	{
	  window_length :: integer(),
	  expired_fun :: fun(),
	  update_fun :: fun(),
	  external_state :: any(),
	  current_window :: [],
	  listener :: pid()
	}).

%%%===================================================================
%%% API
%%%===================================================================
%% @doc
%%  Updates the current window with new data
%% @end
update(Instance,Data) ->
    gen_server:cast(Instance,{incoming_data,Data}).

%% @doc Returns the data in the current window
current_window(Instance) ->
    gen_server:call(Instance,current_state).	

%% @doc Returns the data in the current external state
external_state(Instance) ->	     
    gen_server:call(Instance, external_state).
%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%--------------------------------------------------------------------
-spec start_link(WinSize :: integer(), Listener :: pid()) -> {ok,pid()}| ignore | {error,Error :: any()}.
start_link(WinSize, Listener) ->
    Ref = erlang:make_ref(),
    Id = list_to_atom(erlang:ref_to_list(Ref)),
    gen_server:start_link({local, Id}, ?MODULE, [{WinSize, Listener}], []).


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([{WinSize, Listener}]) ->
    
    {ok, #state
     {
       window_length=WinSize, 
       current_window = [],
       listener = Listener
     }
    }.

handle_call(current_window, _From, #state{current_window = Window} = State) ->
    {reply, Window, State};

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast({incoming_data, Data}, #state{current_window = W, window_length = Size, listener = Listener} = State) ->
    NewWin = lists:append(W,[Data]),
    Exp = process_expired(NewWin, Size),
    case length(Exp) of
	Size ->
	    Listener ! {window, Exp, self()};
	_ ->
	    void
    end,		
    {noreply,State#state{current_window = Exp}};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
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
    Remove = length(Data) - WinSize,
    case Remove > 0 of
	true ->
	    lists:sublist(Data,Remove);
	false ->
	    []
    end.

%% @doc
%%   Removes each expired element and updates the window.
%%   Runs the expiredfun for each expired item and updates the external_state if needed.
%% @end
process_expired(Window, Size) ->
    ExpiredData = get_old_data(Window,Size),
    case ExpiredData of
	[] ->
	    Window;
	_ ->
	    lists:subtract(Window,ExpiredData)
    end.
