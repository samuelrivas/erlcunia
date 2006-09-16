%%%-------------------------------------------------------------------
%%% File    : player.erl
%%% Author  : Samuel Rivas <samuel@lambdastream.com>
%%% Description : Erlcunia's lesson player
%%%
%%% Created : 16 Sep 2006 by Samuel Rivas <samuel@lambdastream.com>
%%%-------------------------------------------------------------------
-module(erlcunia.lesson.player).

-import(gen_server).
-import(file).
-import(random).
-import(lists).
-import(erlcunia.util).

-behaviour(gen_server).

%% API
-export([start_link/0, load_lesson/1, play/0, get_range/0,
	 get_last_question/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-record(state, {
	  range = {48,72},
	  lesson = undefined,
	  last_question = undefined	% {Test, Pitch}
	 }).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

load_lesson(File) ->
    call({load_lesson, File}).

%% Returns the tag of the test being played
play() ->
    call(play).

get_range() ->
    call(get_range).

get_last_question() ->
    call(get_last_question).

call(Msg) ->
    gen_server:call(?MODULE, Msg).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init([]) ->
    %% Seed the pseudorandom number generator
    {A, B, C} = erlang:now(),
    random:seed(A, B, C),
    {ok, #state{}}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call({load_lesson, File}, _From, State) ->
    {reply, ok, State#state{lesson = load_lesson_file(File)}};

handle_call(play, _From, State) ->
    {Cunia, Tag} = choose_test(State#state.lesson),
    Pitch = choose_pitch(State#state.range),
    play({Cunia, Tag}, Pitch),
    {reply, {Tag, Pitch}, State#state{last_question = {Tag, Pitch}}};

handle_call(get_range, _From, State) ->
    {reply, State#state.range, State};

handle_call(get_last_question, _From, State) ->
    {reply, State#state.last_question, State};
    
handle_call(_Request, _From, State) ->
    {reply, {error, wrong_call}, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%% Internal functions
%%--------------------------------------------------------------------

load_lesson_file(File) ->
    case file:consult(File) of
	{ok, Lesson} ->
	    Lesson;
	{error, Reason} ->
	    erlang:error(file:format_error(Reason))
    end.


choose_test(Lesson) ->
    Questions = util:find(questions, Lesson),
    Rand = random:uniform(length(Questions)),
    lists:nth(Rand, Questions).

choose_pitch({Min, Max}) ->
    case Min =/= Max of
	true ->
	    Min + random:uniform(Max - Min + 1) - 1;
	false ->
	    Min
    end.
    
play(Test, Pitch) ->
    {_Cunia, Tag} = Test,
    .io:format("Playing ~p from ~p~n", [Tag, Pitch]).
