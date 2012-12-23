%%%-------------------------------------------------------------------
%%% @author nisbus <nisbus@gmail.com>
%%% @copyright nisbus (C) 2012, 
%%% @doc
%%%   This should be a collection of common functions that one would
%%%   use to e.g. aggregate data.
%%% @end
%%% Created : 23 Dec 2012 by nisbus <nisbus@gmail.com>
%%%-------------------------------------------------------------------
-module(common_functions).
-export([is_defined_function/1]).
%% API
-export([sum/0,moving_average/0,standard_deviation_sample/0,standard_deviation_population/0]).
%%%===================================================================
%%% API
%%%===================================================================

is_defined_function(FunName) when is_atom(FunName)->
    Info = common_functions:module_info(exports),
    FunList = lists:map(fun({Fun,_Arity}) ->
		      Fun
	      end,Info),
    lists:member(FunName, FunList);

is_defined_function(FunName) when is_list(FunName)->
    is_defined_function(list_to_atom(FunName));
    
is_defined_function(FunName) when is_binary(FunName)->
    is_defined_function(binary_to_list(FunName));
is_defined_function(_) ->
    false.
    
moving_average() ->
    fun(X,Y) ->
	    case Y of
		{A,C} ->
		    {((A*C+X)/ (C+1)),C+1};  
		A ->
		    {((X+A) / 2),2}
	    end
    end.

sum() ->
    fun(X,Y) ->
	    X + Y
    end.

standard_deviation_sample() ->
    fun(List) ->
	    C = length(List),
	    S = lists:foldl(fun(X,Sum) -> X+Sum end, 0, List),	    
	    Ave = S/C,
	    A = lists:map(fun(X) -> math:pow(X -Ave,2)/(C-1) end, List),
	    S1 = lists:foldl(fun(X,Sum) -> X+Sum end, 0, A),
	    math:sqrt(S1)
    end.
	    
standard_deviation_population() ->
    fun(List) ->
	    C = length(List),
	    S = lists:foldl(fun(X,Sum) -> X+Sum end, 0, List),	    
	    Ave = S/C,
	    A = lists:map(fun(X) -> math:pow(X -Ave,2) end, List),
	    S1 = lists:foldl(fun(X,Sum) -> X+Sum end, 0, A),
	    math:sqrt(S1)
    end.
