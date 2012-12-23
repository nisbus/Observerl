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
-export([run/0, run_named/0, run_timed_window/0, run_sized_window/0, moving_average/0, transform/0, common_fun/0]).

%%%===================================================================
%%% API
%%%===================================================================
run() ->
    {ok, _Obs} = observer:start_link(),
    S = #subscription{filters = "fun(X) -> X < 30 end.", aggregate = "fun(X,Y) -> X+Y end."},
    observer:subscribe(S),
    observer:on_next(20),
    observer:on_next(30),
    observer:on_next(25),
    observer:unsubscribe(self()),
    observer:stop().

run_named() ->
    Name = 'test_observer',
    {ok, _Obs} = observer:start_link(Name),
    S = #subscription{filters = "fun(X) -> X < 30 end.", aggregate = "fun(X,Y) -> X+Y end."},
    observer:subscribe(S,Name),
    observer:on_next(20,Name),
    observer:on_next(30,Name),
    observer:on_next(25,Name),
    observer:unsubscribe(self(), Name),
    observer:stop(Name).
    
run_timed_window() ->
    observer:start_link(),
    S = #subscription{window = {timed, {0,0,1}, 100, self()}}, 
    observer:subscribe(S),
    observer:on_next(20),
    timer:sleep(500),
    observer:on_next(20),
    timer:sleep(500),
    observer:on_next(20),
    timer:sleep(500),
    observer:on_next(20),
    timer:sleep(500),
    observer:on_next(20),
    timer:sleep(500),
    observer:unsubscribe(self()),
    observer:stop().

    
run_sized_window() ->
    observer:start_link(),
    S = #subscription{window = {sized, 5, self()}}, 
    observer:subscribe(S),
    observer:on_next(20),
    observer:on_next(20),
    observer:on_next(20),
    observer:on_next(20),
    observer:on_next(20),
    observer:on_next(20),
    observer:on_next(20),
    observer:on_next(20),
    observer:on_next(20),
    observer:on_next(20),
    observer:on_next(20),
    observer:unsubscribe(self()),
    observer:stop().

moving_average() ->
    observer:start_link(),

    S = #subscription{aggregate = "fun(X,Y) ->
					  case Y of
					      {A,C} ->
						  {((A*C+X)/ (C+1)),C+1};  
					      A ->
						  {((X+A) / 2),2}
					  end
				  end."},
    observer:subscribe(S),
    observer:on_next(20),
    observer:on_next(21),
    observer:on_next(22),
    observer:on_next(23),
    observer:on_next(24),
    observer:unsubscribe(self()),
    observer:stop().
    
transform() ->
    observer:start_link(),

    S = #subscription{aggregate = "fun(X,Y) ->
					  case Y of
					      {A,C} ->
						  {((A*C+X)/ (C+1)),C+1};  
					      A ->
						  {((X+A) / 2),2}
					  end
				  end.",
		     transform = "fun(V) -> case V of {A,C} -> A; _ -> V end end."},
    observer:subscribe(S),
    observer:on_next(20),
    observer:on_next(21),
    observer:on_next(22),
    observer:on_next(23),
    observer:on_next(24),
    observer:unsubscribe(self()),
    observer:stop().

common_fun() ->
    observer:start_link(),

    S = #subscription{aggregate = "moving_average",
		     transform = "fun(V) -> case V of {A,C} -> A; _ -> V end end."},
    observer:subscribe(S),
    observer:on_next(20),
    observer:on_next(21),
    observer:on_next(22),
    observer:on_next(23),
    observer:on_next(24),
    observer:unsubscribe(self()),
    observer:stop().
    
