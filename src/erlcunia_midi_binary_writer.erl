%%%-------------------------------------------------------------------
%%% File    : binary_writer.erl
%%% Author  : Samuel Rivas <samuel@lambdastream.com>
%%% Description : Creates MIDI binaries
%%%
%%% Created :  2 Jul 2006 by Samuel Rivas <samuel@lambdastream.com>
%%%-------------------------------------------------------------------
-module(erlcunia_midi_binary_writer).

-export([header/3, track/1, track_name/1, end_of_track/0, note_off/4,
	 note_on/4, tempo/2]).

%% Format = 0 | 1 | 2
%% Tracks = integer() > 0
header(Format, Tracks, PPQN) ->
    chunk(<<"MThd">>, <<Format:16, Tracks:16, PPQN:16>>).

%% Content = binary()
track(Content) ->
    chunk(<<"MTrk">>, Content).

%% Name = string()
track_name(Name) ->
    meta_event(0, 3, list_to_binary(Name)).

end_of_track() ->
    meta_event(0, 16#2F, <<>>).

note_on(Delta, Channel, Note, Velocity) ->
    event(Delta, 9, Channel, Note, Velocity).

note_off(Delta, Channel, Note, Velocity) ->
    event(Delta, 8, Channel, Note, Velocity).

tempo(Delta, BPM) ->
    Microseconds = round(60000000/BPM),
    meta_event(Delta, 16#51, <<Microseconds:24>>).

%%%-------------------------------------------------------------------
%%% Module internals
%%%-------------------------------------------------------------------

%% Name = Content = binary()
chunk(Name, Content) ->
    Length = size(Content),
    <<Name/binary, Length:32, Content/binary>>.

%% Write variable length quantities
%% N = integer()
vlq(N) when N =< 16#0FFFFFFF ->
    vlq_binary(<<N:32>>).

vlq_binary(<<0:4, B3:7, B2:7, B1:7, B0:7>>) ->
    vlq_binary(discard_zeroes([B3, B2, B1, B0]), []).

discard_zeroes([X | []]) ->
    [X];
discard_zeroes([0 | T]) ->
    discard_zeroes(T);
discard_zeroes(What) ->
    What.

vlq_binary([H | []], Acc) ->
    list_to_binary(lists:reverse([<<H>> | Acc]));
vlq_binary([H | T], Acc) ->
    vlq_binary(T, [<<1:1, H:7>> | Acc]).


%% Write a generic MIDI event
%% Delta = integer()
%% Statis = Data = binary()
event(Delta, Status, Data) ->
    DeltaBinary = vlq(Delta),
    <<DeltaBinary/binary, Status/binary, Data/binary>>.

%% Write a MIDI meta-event
%% Delta = Type = integer()
%% Data = binary()
meta_event(Delta, Type, Data) ->
    Length = vlq(size(Data)),
    event(Delta, <<16#FF:8>>, <<Type:8, Length/binary, Data/binary>>).

%% Write Channel messages
%% Delta = Status = Channel = Byte0 = integer
event(Delta, Status, Channel, Byte0) when is_integer(Byte0) ->
    event(Delta, Status, Channel, <<Byte0:8>>);
event(Delta, Status, Channel, Data) ->
    event(Delta, <<Status:4, Channel:4>>, Data).

event(Delta, Status, Channel, Byte0, Byte1) ->
    event(Delta, Status, Channel, <<Byte0:8, Byte1:8>>).
