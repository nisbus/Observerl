
-module(observerl_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).
-export([add_observer/1]).
%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    SupFlags = {simple_one_for_one, 1000, 3600},
    Child = {observer, {observer, start_link, []},
	     temporary, 2000, worker, [observer]},
    {ok, {SupFlags,[Child]}}.

%%% @doc
%%%   Starts a new observer
%%% @end
add_observer(Args) ->
    supervisor:start_child(?MODULE, [Args]).
