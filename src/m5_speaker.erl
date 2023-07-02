% SPDX-License-Identifier: MIT
-module(m5_speaker).

-export([
    tone/2,
    tone/3,
    tone/4
]).

%% @doc Play a tone for a given frequency and duration.
%% @param Freq frequency of the tone in Hz
%% @param Duration duration of the tone in milliseconds
%% @returns `true' on success.
-spec tone(Freq :: number(), Duration :: number()) -> boolean().
tone(_Freq, _Duration) ->
    throw(nif_error).

%% @doc Play a tone for a given frequency and duration.
%% @param Freq frequency of the tone in Hz
%% @param Duration duration of the tone in milliseconds
%% @param Channel virtual channel (0-7)
%% @returns `true' on success.
-spec tone(Freq :: number(), Duration :: pos_integer(), Channel :: integer()) -> boolean().
tone(_Freq, _Duration, _Channel) ->
    throw(nif_error).

%% @doc Play a tone for a given frequency and duration.
%% @param Freq frequency of the tone in Hz
%% @param Duration duration of the tone in milliseconds
%% @param Channel virtual channel (0-7)
%% @param StopPrevious if current sound should be stopped
%% @returns `true' on success.
-spec tone(
    Freq :: number(), Duration :: pos_integer(), Channel :: integer(), StopPrevious :: boolean()
) -> boolean().
tone(_Freq, _Duration, _Channel, _StopPrevious) ->
    throw(nif_error).
