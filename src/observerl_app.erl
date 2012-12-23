-module(observerl_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================
%%% @doc Starts the application
start(_StartType, _StartArgs) ->
    observerl_sup:start_link().

%%% @doc Stops the application
stop(_State) ->
    ok.
