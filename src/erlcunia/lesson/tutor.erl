%%%-------------------------------------------------------------------
%%% File    : tutor.erl
%%% Author  : Samuel Rivas <samuel@lambdastream.com>
%%% Description : The Erlcunia's personal tutor.
%%%
%%% Created : 23 Sep 2006 by Samuel Rivas <samuel@lambdastream.com>
%%%-------------------------------------------------------------------
-module(erlcunia.lesson.tutor).

-behaviour(gen_server).

%% API
-export([start_link/0, new_test/0, test_answer/1, get_answer/0, settings/2,
	 get_settings/0, load_lesson/1, get_range/0, set_range/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-import(gen_server).
-import(random).
-import(lists).
-import(erlcunia.lesson).
-import(erlcunia.util).

-record(state, {
	  repeat_test = true,
	  play_wrong = true,
	  right_answer = undefined,
	  all_tests = [],
	  selected_tests = [],
	  range = {36, 72}
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

new_test() ->
    call(new_test).

test_answer(Answer) ->
    call({test_answer, Answer}).

get_answer() ->
    call(get_answer).

get_range() ->
    call(get_range).

set_range(Min, Max) ->
    call({set_range, {Min, Max}}).

settings(RepeatTest, RepeatWrong) ->
    call({settings, RepeatTest, RepeatWrong}).

%% Return {RepeatTest, RepeatWrong} (both boolean)
get_settings() ->
    call(get_settings).

call(Message) ->
    gen_server:call(?MODULE, Message, infinity).

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
    player:load_lesson(File),
    Tests = player:get_tests(), 
    {reply, ok, State#state{all_tests = Tests, selected_tests = Tests}};

handle_call(new_test, _From, State) ->
    Test = random_test(State#state.selected_tests),
    Tone = choose_tone(State#state.range),
    player:play(Test, Tone),
    {reply, ok, State#state{right_answer = {Test, Tone}}};

handle_call({test_answer, Answer}, _From, State) ->
    Response = test_answer(Answer, State),
    {reply, Response, State};

handle_call(get_answer, _From, State) ->
    {reply, State#state.right_answer, State};

handle_call({settings, RepeatTest, PlayWrong}, _From, State) ->
    util:check_boolean(RepeatTest),
    util:check_boolean(PlayWrong),
    {reply, ok, State#state{repeat_test = RepeatTest,
			    play_wrong = PlayWrong}};

handle_call(get_settings, _From, State) ->
    #state{repeat_test = RepeatTest, play_wrong = PlayWrong} = State,
    {reply, {RepeatTest, PlayWrong}, State};
    
handle_call(get_range, _From, State) ->
    {reply, State#state.range, State};

handle_call({set_range, Range}, _From, State) ->
    {reply, ok, State#state{range = Range}};

handle_call(_Request, _From, State) ->
    {reply, {error, bad_call} , State}.

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
%%% Internal functions
%%--------------------------------------------------------------------

test_answer(Answer, State) ->
    case State#state.right_answer of
	{Answer, _Tone} ->
	    true;
	_ ->
	    repeat(Answer, State),
	    false
    end.

repeat(Answer, State) ->
    {Right, Tone} = State#state.right_answer,
    case {State#state.play_wrong, State#state.repeat_test} of
	{true, true} ->
	    player:play([Answer, Right], [{rest, 2}], Tone);
	{true, false} ->
	    player:play(Answer, Tone);
	{false, true} ->
	    player:play(Right, Tone);
	{false, false} ->
	    ok
    end.

random_test(Tests) ->
    Rand = random:uniform(length(Tests)),
    lists:nth(Rand, Tests).

choose_tone({Min, Max}) ->
    case Min =/= Max of
	true ->
	    Min + random:uniform(Max - Min + 1) - 1;
	false ->
	    Min
    end.
    
