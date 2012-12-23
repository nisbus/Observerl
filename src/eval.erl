%%%-------------------------------------------------------------------
%%% @author nisbus <nisbus@gmail.com>
%%% @copyright nisbus (C) 2012, 
%%% @doc
%%%    Takes a string in the format "fun(X) X+1 end" and returns it as
%%%    an executable fun.
%%%    Also supports binary formatted string fun and an actual fun.
%%% @end
%%% Created : 22 Dec 2012 by nisbus <nisbus@gmail.com>
%%%-------------------------------------------------------------------
-module(eval).

%% API
-export([make_fun/1,make_funs/1]).

%%%===================================================================
%%% API
%%%===================================================================

%%%@doc
%%%  Creates a function from a string or binary.
%%%  If input is function it just returns the function.
%%%@end
-spec make_fun(Fun :: undefined | fun() | binary() | string()) -> fun().
make_fun(undefined) ->
    undefined;
make_fun(Fun) when is_function(Fun) ->
    Fun;
make_fun(Fun) when is_binary(Fun) ->
    make_fun(binary_to_list(Fun));
make_fun(Fun) ->
    case common_functions:is_defined_function(Fun) of
	true ->
	    F = list_to_atom(Fun),
	    common_functions:F();
	false ->
	    {ok, F,_} = erl_scan:string(Fun),
	    {ok,Exp} = erl_parse:parse_exprs(F),
	    {_,Fun0,_} = erl_eval:exprs(Exp,erl_eval:new_bindings()),
	    Fun0
    end.

%%%@doc
%%%  Creates a list of functions from a list of strings or binary.
%%%  If input is a function list it just returns the function list.
%%%@end
-spec make_funs(Funs :: undefined | [string()] | [fun()] | [binary()]) -> [fun()].
make_funs(undefined) ->
    [];
make_funs(Funs) ->
    lists:map(fun(X) ->
		      make_fun(X)
	      end,Funs).
