%%%-------------------------------------------------------------------
%%% File    : player.erl
%%% Author  : Samuel Rivas <samuel@lambdastream.com>
%%% Description : Erlcunia's lesson player
%%%
%%% Created : 16 Sep 2006 by Samuel Rivas <samuel@lambdastream.com>
%%%-------------------------------------------------------------------
-module(erlcunia_lesson_player).

-behaviour(gen_server).

%% API
-export([start_link/0, load_lesson/1, play/2, play/3, get_tests/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-record(state, {
	  lesson = undefined
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

get_tests() ->
    call(get_tests).

play(Test, Tone) ->
    call({play, Test, Tone}).

%% Tests = [{Test, Tone}]
%% Divider = cunia()
play(Tests, Divider, Tone) ->
    call({play, Tests, Divider, Tone}).

call(Msg) ->
    gen_server:call(?MODULE, Msg, infinity).

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

handle_call({play, Test, Tone}, _From, State) ->
    Cunia = get_cunia(State#state.lesson, Test),
    play_cunia(Cunia, Tone),
    {reply, ok, State};

handle_call({play, Tests, Divider, Tone}, _From, State) ->
    Cunia = join_tests(Tests, Divider, State#state.lesson),
    play_cunia(Cunia, Tone),
    {reply, ok, State};

handle_call(get_tests, _From, State) ->
    {reply, get_tests(State#state.lesson), State};

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

get_cunia(Lesson, Test) ->
    find_test(Test, erlcunia_util:find(tests, Lesson)).

find_test(Test, Tests) ->
    case lists:keysearch(Test, 2, Tests) of
	{value, {Cunia, Test}} ->
	    Cunia;
	false ->
	    erlang:error({no_test, Test})
    end.

play_cunia(Cunia, Tone) ->
    erlcunia.midi.player:play(transpose(Cunia, Tone)).

transpose([], _Tone) ->
    [];
transpose([Event | T], Tone) ->
    [transpose_event(Event, Tone) | transpose(T, Tone)].

transpose_event({notes, Notes, Length}, Tone) ->
    {notes, [Note + Tone || Note <- Notes], Length};
transpose_event(Event, _Tone) ->
    Event.

join_tests(Tests, Divider, Lesson) ->
    join_tests2(Tests, Divider, erlcunia_util:find(tests, Lesson)).

join_tests2([], _Divider, _Tests) ->
    [];
join_tests2([First | T], Divider, Tests) ->
    lists:flatten([find_test(First, Tests)
		   | [[Divider, find_test(Test, Tests)] || Test <- T]]).

get_tests(Lesson) ->
    [Test || {_Cunia, Test} <- erlcunia_util:find(tests, Lesson)].
