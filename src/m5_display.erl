% SPDX-License-Identifier: MIT
-module(m5_display).

-export([
    set_epd_mode/1,
    height/0,
    width/0,
    get_rotation/0,
    set_rotation/1,
    set_text_size/1,
    start_write/0,
    end_write/0,
    fill_rect/5,
    print/1,
    println/1, println/0
]).

-spec set_epd_mode(fastest) -> ok.
set_epd_mode(_Mode) ->
    throw(nif_error).

-spec width() -> integer().
width() ->
    throw(nif_error).

-spec height() -> integer().
height() ->
    throw(nif_error).

-spec get_rotation() -> integer().
get_rotation() ->
    throw(nif_error).

-spec set_rotation(integer()) -> ok.
set_rotation(_Rotation) ->
    throw(nif_error).

-spec set_text_size(integer()) -> ok.
set_text_size(_TextSize) ->
    throw(nif_error).

-spec start_write() -> ok.
start_write() ->
    throw(nif_error).

-spec end_write() -> ok.
end_write() ->
    throw(nif_error).

-spec fill_rect(integer(), integer(), integer(), integer(), integer()) -> ok.
fill_rect(_X, _Y, _W, _H, _Color) ->
    throw(nif_error).

-spec print(binary()) -> ok.
print(_Binary) ->
    throw(nif_error).

-spec println(binary()) -> ok.
println(_Binary) ->
    throw(nif_error).

-spec println() -> ok.
println() ->
    throw(nif_error).
