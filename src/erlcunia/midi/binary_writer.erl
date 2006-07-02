%%%-------------------------------------------------------------------
%%% File    : binary_writer.erl
%%% Author  : Samuel Rivas <samuel@lambdastream.com>
%%% Description : Creates MIDI binaries
%%%
%%% Created :  2 Jul 2006 by Samuel Rivas <samuel@lambdastream.com>
%%%-------------------------------------------------------------------
-module(midi.binary_writer).

-export([write_header/3]).
-import(lists).

%% Format = 0 | 1 | 2
%% Tracks = integer() > 0
write_header(Format, Tracks, PPQN) ->
    write_chunk(<<"MThd">>, <<Format:16, Tracks:16, PPQN:16>>).

%%%-------------------------------------------------------------------
%%% Module internals
%%%-------------------------------------------------------------------

%% Name = Content = binary()
write_chunk(Name, Content) ->
    Length = size(Content),
    <<Name/binary, Length:32, Content/binary>>.
