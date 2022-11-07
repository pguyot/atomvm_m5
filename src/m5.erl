% SPDX-License-Identifier: MIT
-module(m5).

-export([begin_/1, get_board/0, update/0]).

-spec begin_(list(property:property())) -> ok.
begin_(_Cfg) ->
    throw(nif_error).

%%-----------------------------------------------------------------------------
%% @returns the board
%% @doc     Return automatically identified board using M5.getBoard().
%% @end
%%-----------------------------------------------------------------------------
-spec get_board() -> atom() | undefined.
get_board() ->
    throw(nif_error).

-spec update() -> ok.
update() ->
    throw(nif_error).
