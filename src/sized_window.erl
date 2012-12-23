%%%-------------------------------------------------------------------
%%% @author nisbus <nisbus@gmail.com>
%%% @copyright nisbus (C) 2012, nisbus
%%% @doc
%%%   sized window notifies the subscriber when the window has reached
%%%   the number of elements specified by the size.
%%% @end
%%% Created : 18 Nov 2012 by nisbus <nisbus@gmail.com>
%%%-------------------------------------------------------------------
-module(sized_window).

-behaviour(gen_server).

%% API
-export([start_link/2, update/2,current_window/1, stop/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 


-record(state, 
	{
	  window_length :: integer(),
	  current_window :: [],
	  listener :: pid()
	}).

%%%===================================================================
%%% API
%%%===================================================================
%%% @doc
%%%  Updates the current window with new data
%%% @end
update(Instance,Data) ->
    gen_server:cast(Instance,{incoming_data,Data}).

%%% @doc Returns the data in the current window
current_window(Instance) ->
    gen_server:call(Instance,current_state).	

%%%@doc stops the window
stop(Instance) ->
    gen_server:cast(Instance,stop).
%%%--------------------------------------------------------------------   
%%% @doc
%%% Starts the server
%%%   WinSize is the number of elements to keep in the window.
%%%   Listener is the pid of the subscriber.
%%% @end
%%%--------------------------------------------------------------------
-spec start_link(WinSize :: integer(), Listener :: pid()) -> {ok,pid()}| ignore | {error,Error :: any()}.
start_link(WinSize, Listener) ->
    Ref = erlang:make_ref(),
    Id = list_to_atom(erlang:ref_to_list(Ref)),
    gen_server:start_link({local, Id}, ?MODULE, [{WinSize, Listener}], []).


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
%%% @hidden
init([{WinSize, Listener}]) ->
    
    {ok, #state
     {
       window_length=WinSize, 
       current_window = [],
       listener = Listener
     }
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
handle_cast({incoming_data, Data}, #state{current_window = W, window_length = Size, listener = Listener} = State) ->
    NewWin = lists:append(W,[Data]),
    Exp = process_expired(NewWin, Size),
    case length(Exp) of
	Size ->
	    io:format("Sending window because of size"),
	    Listener ! {window, Exp, self()};
	_ ->
	    void
    end,		
    {noreply,State#state{current_window = Exp}};

%%% @hidden
handle_cast(_Msg, State) ->
    {noreply, State}.

%%% @hidden
handle_info(_Info, State) ->
    {noreply, State}.

%%% @hidden
terminate(_Reason, _State) ->
    ok.

%%% @hidden
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%%Internal functions
%%%===================================================================

%%% @hidden
get_old_data(Data, WinSize) ->
    Remove = length(Data) - WinSize,
    case Remove > 0 of
	true ->
	    lists:sublist(Data,Remove);
	false ->
	    []
    end.

%%% @hidden
process_expired(Window, Size) ->
    ExpiredData = get_old_data(Window,Size),
    case ExpiredData of
	[] ->
	    Window;
	_ ->
	    lists:subtract(Window,ExpiredData)
    end.
