%%%-------------------------------------------------------------------
%%% @author nisbus <nisbus@gmail.com>
%%% @copyright (C) 2012, 
%%% @doc
%%%
%%% @end
%%% Created : 23 Dec 2012 by nisbus <nisbus@gmail.com>
%%%-------------------------------------------------------------------
-module(test_run).
-include("../include/types.hrl").
%% API
-export([run/0, run_named/0]).

%%%===================================================================
%%% API
%%%===================================================================
run() ->
    {ok, _Obs} = observer:start_link(),
    S = #observer_state{filters = "fun(X) -> X < 30 end.", aggregate = "fun(X,Y) -> X+Y end."},
    observer:subscribe(S),
    observer:on_next(20),
    observer:on_next(30),
    observer:on_next(25),
    observer:unsubscribe(self()),
    observer:stop().

run_named() ->
    Name = 'test_observer',
    {ok, _Obs} = observer:start_link(Name),
    S = #observer_state{filters = "fun(X) -> X < 30 end.", aggregate = "fun(X,Y) -> X+Y end."},
    observer:subscribe(S,Name),
    observer:on_next(20,Name),
    observer:on_next(30,Name),
    observer:on_next(25,Name),
    observer:unsubscribe(self(), Name),
    observer:stop(Name).
    
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

%%%===================================================================
%%% Internal functions
%%%===================================================================
