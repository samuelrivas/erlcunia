%%%-------------------------------------------------------------------
%%% File    : player.erl
%%% Author  : Samuel Rivas <samuel@lambdastream.com>
%%% Description : Plays midi sounds
%%%
%%% Created : 25 Jul 2006 by Samuel Rivas <samuel@lambdastream.com>
%%%-------------------------------------------------------------------
-module(erlcunia.midi.player).

-behaviour(gen_server).

%% API
-export([start_link/0, play/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-import(erlcunia.midi).
-import(gen_server).
-import(lists).
-import(dict).

-record(state, {
	  header		% binary()
	 }).

-define(PPQN, 120).
-define(PLAYER, "/usr/bin/timidity -").

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% See Notes.txt for an explanation of the Cunia notation
%% Note that cunia to midi translation is performed on the client side,
%% the player server adds the midi header and calls to the midi player.
play(Cunia) ->
    gen_server:call(?MODULE, {play, cunia2midi_track(Cunia)}, infinity).

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
    %% Midi file with one track and ?PPQN pulses per quarter note
    {ok, #state{header = binary_writer:header(0, 1, ?PPQN)}}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call({play, Binary}, _From, State) ->
    Result = play(Binary, State),
    {reply, Result, State};
handle_call(_Request, _From, State) ->
    {reply, {error, bad_call}, State}.

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
play(Track, #state{header = Header}) ->
    Port = open_port({spawn, ?PLAYER}, [stream, binary, eof]),
    port_command(Port, list_to_binary([Header, Track])),
    receive
	{Port, eof} ->
	    port_close(Port)
    end.

cunia2midi_track(Cunia) ->
    Events = cunia2events(Cunia, 0, event_dict()),
    TrackContent = events2midi(Events),
    binary_writer:track(TrackContent).

%% Events = dict({Note, [Event]})
%% Note = integer() -- All the notes from 0 to 127 are allocated with an
%%                     initial empty event list
%% Event = {note_on, Note, Pulse} | {note_off, Pulse}
%% Pulse = integer()
cunia2events([{notes, Notes, Length} | T], Pulse, Events) ->
    NextPulse = add_length(Pulse, Length),
    NewEvents = add_note_events(Notes, Pulse, NextPulse, Events),
    cunia2events(T, NextPulse, NewEvents);
cunia2events([{rest, Length} | T], Pulse, Events) ->
    cunia2events(T, add_length(Pulse, Length), Events);
cunia2events([], _Pulse, Events) ->

    %% Create a sorted list of events
    NoteEvents = [E || {_Note, E} <- dict:to_list(Events), E /= []],
    lists:keysort(3, lists:flatten(NoteEvents)).

add_note_events(Notes, OnPulse, OffPulse, Events) ->
    F = fun({Note, tie}, Acc) ->
		D = remove_last_off_event(Note, OnPulse, Acc),
		dict:append(Note, {note_off, Note, OffPulse}, D);
	   (Note, Acc) ->
		dict:append_list(Note, [{note_on, Note, OnPulse},
					{note_off, Note, OffPulse}],
				 Acc)
	end,
    lists:foldl(F, Events, Notes).

remove_last_off_event(Note, TiePulse, Events) ->
    NoteEvents = dict:fetch(Note, Events),
    dict:store(Note, remove_last_off_event(TiePulse, NoteEvents), Events).

remove_last_off_event(TiePulse, [{note_off, _, TiePulse} | []]) ->
    [];
remove_last_off_event(TiePulse, [H | T]) ->
    [H | remove_last_off_event(TiePulse, T)];
remove_last_off_event(_, _) ->
    exit(bad_tie).

add_length(_Pulse, {tuplet, _Divisions, _BaseLength}) ->
    throw(unimplemented);
add_length(Pulse, BaseLength) ->
    Pulse + round(?PPQN * 4/BaseLength).

event_dict() ->
    F = fun(N, Dict) ->
		dict:store(N, [], Dict)
	end,
    lists:foldl(F, dict:new(), lists:seq(0, 127)).

events2midi(Events) ->
    list_to_binary([events2midi(Events, 0), binary_writer:end_of_track()]).

events2midi([{Type, Note, Pulse} | T], LastPulse) ->
    Binary = case Type of
		 note_on ->
		     binary_writer:note_on(Pulse - LastPulse, 0, Note, 16#7F);
		 note_off ->
		     binary_writer:note_off(Pulse - LastPulse, 0, Note, 16#7F)
	     end,
    [Binary | events2midi(T, Pulse)];
events2midi([], _Pulse) ->
    [].
