%%%-------------------------------------------------------------------
%%% @author  nisbus <nisbus@gmail.com>
%%% @copyright (C) 2012, 
%%% @doc
%%%  this module runs a list of filters on a value and returns true 
%%%  if all the filters pass, otherwise false
%%% @end
%%% Created : 21 Dec 2012 by nisbus <nisbus@gmail.com>
%%%-------------------------------------------------------------------
-module(filter).
-export([match/2]).

match(undefined,_) ->
    true;
match([],_) ->
    true;
match(Funs, Value) ->
    Matches = lists:map(fun(F) ->
				F(Value)
			end, Funs),
    lists:all(fun(X) ->
		      X =:= true
	      end, Matches).
