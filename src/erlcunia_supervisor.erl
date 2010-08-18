%%%-------------------------------------------------------------------
%%% Copyright 2006, 2007, 2010 Samuel Rivas <samuelrivas@gmail.com>
%%%
%%% This file is part of Erlcunia.
%%%
%%% Erlcunia is free software: you can redistribute it and/or modify it under
%%% the terms of the GNU General Public License as published by the Free
%%% Software Foundation, either version 3 of the License, or (at your option)
%%% any later version.
%%%
%%% Erlcunia is distributed in the hope that it will be useful, but WITHOUT ANY
%%% WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
%%% FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
%%% details.
%%%
%%% You should have received a copy of the GNU General Public License along with
%%% Erlcunia.  If not, see <http://www.gnu.org/licenses/>.
%%%-------------------------------------------------------------------
%%%-------------------------------------------------------------------
%%% File    : supervisor.erl
%%% Author  : Samuel Rivas <samuelrivas@gmail.com>
%%% Description : Erlcunia's main supervisor
%%%
%%% Created : 23 Sep 2006 by Samuel Rivas <samuelrivas@gmail.com>
%%%-------------------------------------------------------------------
-module(erlcunia_supervisor).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the supervisor
%%--------------------------------------------------------------------
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================
%%--------------------------------------------------------------------
%% Func: init(Args) -> {ok,  {SupFlags,  [ChildSpec]}} |
%%                     ignore                          |
%%                     {error, Reason}
%% Description: Whenever a supervisor is started using
%% supervisor:start_link/[2,3], this function is called by the new process
%% to find out about restart strategy, maximum restart frequency and child
%% specifications.
%%--------------------------------------------------------------------
init([]) ->
    {ok,{{one_for_one,10,1}, children()}}.

%%====================================================================
%% Internal functions
%%====================================================================
children() ->
    [childspec(X) || X <- [erlcunia_lesson_player,
			   erlcunia_lesson_tutor,
			   erlcunia_midi_player]].

childspec(Name) ->
    {Name, {Name, start_link, []}, permanent, 2000, worker, [Name]}.
