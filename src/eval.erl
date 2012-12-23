%%%-------------------------------------------------------------------
%%% @author nisbus <nisbus@gmail.com>
%%% @copyright (C) 2012, 
%%% @doc
%%%    Takes a string in the format "fun(X) X+1 end" and returns it as
%%%    an executable fun.
%%% @end
%%% Created : 22 Dec 2012 by nisbus <nisbus@gmail.com>
%%%-------------------------------------------------------------------
-module(eval).

%% API
-export([make_fun/1,make_funs/1]).

%%%===================================================================
%%% API
%%%===================================================================
make_fun(S) when is_binary(S) ->
    make_fun(binary_to_list(S));

make_fun(S) ->
    {ok, F,_} = erl_scan:string(S),
    {ok,Exp} = erl_parse:parse_exprs(F),
    {_,Fun,_} = erl_eval:exprs(Exp,erl_eval:new_bindings()),
    Fun.

make_funs(undefined) ->
    [];
make_funs(S) ->
    lists:map(fun(X) ->
		      make_fun(X)
	      end,S).
