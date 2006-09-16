%%%-------------------------------------------------------------------
%%% File    : util.erl
%%% Author  : Samuel Rivas <samuel@lambdastream.com>
%%% Description : Common utilities
%%%
%%% Created : 16 Sep 2006 by Samuel Rivas <samuel@lambdastream.com>
%%%-------------------------------------------------------------------
-module(erlcunia.util).

-import(lists).

-export([find/2]).

find(Key, List) ->
    case lists:keysearch(Key, 1, List) of
	{value, {Key, Value}} ->
	    Value;
	false ->
	    erlang:error({not_found, Key, List})
    end.
