% SPDX-License-Identifier: MIT
-module(m5).

-export([begin_/1, get_board/0, update/0]).

%% @doc Power on the board and initialize M5 library.
%% @param _Cfg configuration
-spec begin_(list(property:property())) -> ok.
begin_(_Cfg) ->
    throw(nif_error).

%% @doc     Return automatically identified board using M5.getBoard().
%% @returns the board
-spec get_board() -> atom() | undefined.
get_board() ->
    throw(nif_error).

%% @doc Update function to call in loops.
%%
%% Handles touch and buttons.
-spec update() -> ok.
update() ->
    throw(nif_error).
