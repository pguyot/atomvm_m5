% SPDX-License-Identifier: MIT
-module(m5_display).

-export([
    set_epd_mode/1,
    set_brightness/1,
    sleep/0,
    wakeup/0,
    power_save/1,
    power_save_on/0,
    power_save_off/0,

    set_color/1,
    set_color/3,
    set_raw_color/1,
    get_raw_color/0,
    set_base_color/1,
    get_base_color/0,

    start_write/0,
    end_write/0,

    write_pixel/2,
    write_pixel/3,
    write_fast_vline/3,
    write_fast_vline/4,
    write_fast_hline/3,
    write_fast_hline/4,
    write_fill_rect/4,
    write_fill_rect/5,
    write_fill_rect_preclipped/4,
    write_fill_rect_preclipped/5,
    write_color/2,
    push_block/2,
    draw_pixel/2,
    draw_pixel/3,
    draw_fast_vline/3,
    draw_fast_vline/4,
    draw_fast_hline/3,
    draw_fast_hline/4,
    fill_rect/4,
    fill_rect/5,
    draw_rect/4,
    draw_rect/5,
    draw_round_rect/5,
    draw_round_rect/6,
    fill_round_rect/5,
    fill_round_rect/6,
    draw_circle/3,
    draw_circle/4,
    fill_circle/3,
    fill_circle/4,
    draw_ellipse/4,
    draw_ellipse/5,
    fill_ellipse/4,
    fill_ellipse/5,
    draw_line/4,
    draw_line/5,
    draw_triangle/6,
    draw_triangle/7,
    fill_triangle/6,
    fill_triangle/7,
    draw_bezier/6,
    draw_bezier/7,
    draw_bezier/8,
    draw_bezier/9,

    fill_screen/0,
    fill_screen/1,
    clear/0,
    clear/1,

    width/0,
    height/0,

    wait_display/0,
    display_busy/0,
    set_auto_display/1,

    get_rotation/0,
    set_rotation/1,

    set_clip_rect/4,
    get_clip_rect/0,
    clear_clip_rect/0,
    set_scroll_rect/4,
    get_scroll_rect/0,
    clear_scroll_rect/0,

    get_cursor/0,
    set_cursor/2,
    set_text_size/1,
    set_text_size/2,

    font_height/0,
    font_width/0,

    draw_string/3,
    draw_center_string/3,
    draw_right_string/3,

    print/1,
    println/1,
    println/0
]).

-type rgb888() :: 0..16777216.
-type rgb565() :: 0..65536.
-type color() ::
    rgb888() | {rgb888, rgb888()} | {rgb565, rgb565()} | {rgb, {0..255, 0..255, 0..255}}.
-type raw_color() :: integer().
-type rotation() :: 0..3.

%% @doc Set EPD mode.
%% @param _Mode mode (currently only fastest is supported)
-spec set_epd_mode(fastest) -> ok.
set_epd_mode(_Mode) ->
    throw(nif_error).

%% @doc Set brightness
%% @param _Brightness brightness level. 0 means screen is off.
-spec set_brightness(0..255) -> ok.
set_brightness(_Brightness) ->
    throw(nif_error).

%% @doc Set display to sleep.
%% This function first set brightness to 0.
-spec sleep() -> ok.
sleep() ->
    throw(nif_error).

%% @doc Wake display up.
%% This function restores brightness.
-spec wakeup() -> ok.
wakeup() ->
    throw(nif_error).

%% @doc Set power save flag
-spec power_save(boolean()) -> ok.
power_save(_Flag) ->
    throw(nif_error).

%% @doc Set power save flag to true
-spec power_save_on() -> ok.
power_save_on() ->
    throw(nif_error).

%% @doc Set power save flag to false
-spec power_save_off() -> ok.
power_save_off() ->
    throw(nif_error).

%% @doc Set the color.
-spec set_color(color()) -> ok.
set_color(_Color) ->
    throw(nif_error).

%% @doc Set the color.
-spec set_color(0..255, 0..255, 0..255) -> ok.
set_color(_R, _G, _B) ->
    throw(nif_error).

%% @doc Set raw color
-spec set_raw_color(raw_color()) -> ok.
set_raw_color(_Color) ->
    throw(nif_error).

%% @doc Get raw color
-spec get_raw_color() -> raw_color().
get_raw_color() ->
    throw(nif_error).

%% @doc Set base color.
%% Base color is used by `clear/0' and `scroll/2'.
-spec set_base_color(rgb888()) -> ok.
set_base_color(_Color) ->
    throw(nif_error).

%% @doc Get base color
-spec get_base_color() -> rgb888().
get_base_color() ->
    throw(nif_error).

%% @doc Start writing on display.
%% Should be balanced with `end_write'
-spec start_write() -> ok.
start_write() ->
    throw(nif_error).

%% @doc End writing on display.
%% Should be balanced with `start_write'
-spec end_write() -> ok.
end_write() ->
    throw(nif_error).

%% @doc Write a pixel with current color.
%% Should be enclosed with `start_write' and `end_write'.
-spec write_pixel(integer(), integer()) -> ok.
write_pixel(_X, _Y) ->
    throw(nif_error).

%% @doc Set color and write a pixel.
%% Should be enclosed with `start_write' and `end_write'.
-spec write_pixel(integer(), integer(), rgb888()) -> ok.
write_pixel(_X, _Y, _Color) ->
    throw(nif_error).

%% @doc Write a vertical line with current color.
%% Should be enclosed with `start_write' and `end_write'.
-spec write_fast_vline(integer(), integer(), integer()) -> ok.
write_fast_vline(_X, _Y, _H) ->
    throw(nif_error).

%% @doc Set color and write a vertical line.
%% Should be enclosed with `start_write' and `end_write'.
-spec write_fast_vline(integer(), integer(), integer(), rgb888()) -> ok.
write_fast_vline(_X, _Y, _H, _Color) ->
    throw(nif_error).

%% @doc Write a horizontal line with current color.
%% Should be enclosed with `start_write' and `end_write'.
-spec write_fast_hline(integer(), integer(), integer()) -> ok.
write_fast_hline(_X, _Y, _W) ->
    throw(nif_error).

%% @doc Set color and write a horizontal line.
%% Should be enclosed with `start_write' and `end_write'.
-spec write_fast_hline(integer(), integer(), integer(), rgb888()) -> ok.
write_fast_hline(_X, _Y, _W, _Color) ->
    throw(nif_error).

%% @doc Fill a rectangle with current color.
%% Should be enclosed with `start_write' and `end_write'.
-spec write_fill_rect(integer(), integer(), integer(), integer()) -> ok.
write_fill_rect(_X, _Y, _W, _H) ->
    throw(nif_error).

%% @doc Set color and fill a rectangle.
%% Should be enclosed with `start_write' and `end_write'.
-spec write_fill_rect(integer(), integer(), integer(), integer(), rgb888()) -> ok.
write_fill_rect(_X, _Y, _W, _H, _Color) ->
    throw(nif_error).

%% @doc Fill a rectangle with current color, without clipping.
%% Should be enclosed with `start_write' and `end_write'.
-spec write_fill_rect_preclipped(integer(), integer(), integer(), integer()) -> ok.
write_fill_rect_preclipped(_X, _Y, _W, _H) ->
    throw(nif_error).

%% @doc Set color and fill a rectangle, without clipping.
%% Should be enclosed with `start_write' and `end_write'.
-spec write_fill_rect_preclipped(integer(), integer(), integer(), integer(), rgb888()) -> ok.
write_fill_rect_preclipped(_X, _Y, _W, _H, _Color) ->
    throw(nif_error).

%% @doc Set color and push block.
%% Should be enclosed with `start_write' and `end_write'.
-spec write_color(rgb888(), integer()) -> ok.
write_color(_Color, _Length) ->
    throw(nif_error).

%% @doc Set color and push block.
%% Calls `start_write' and `end_write'.
-spec push_block(rgb888(), integer()) -> ok.
push_block(_Color, _Length) ->
    throw(nif_error).

%% @doc Draw a pixel with current color.
%% Calls `start_write' and `end_write'.
-spec draw_pixel(integer(), integer()) -> ok.
draw_pixel(_X, _Y) ->
    throw(nif_error).

%% @doc Set color and draw a pixel.
%% Calls `start_write' and `end_write'.
-spec draw_pixel(integer(), integer(), rgb888()) -> ok.
draw_pixel(_X, _Y, _Color) ->
    throw(nif_error).

%% @doc Draw a vertical line with current color.
%% Calls `start_write' and `end_write'.
-spec draw_fast_vline(integer(), integer(), integer()) -> ok.
draw_fast_vline(_X, _Y, _H) ->
    throw(nif_error).

%% @doc Set color and draw a vertical line.
%% Calls `start_write' and `end_write'.
-spec draw_fast_vline(integer(), integer(), integer(), rgb888()) -> ok.
draw_fast_vline(_X, _Y, _H, _Color) ->
    throw(nif_error).

%% @doc Draw a horizontal line with current color.
%% Calls `start_write' and `end_write'.
-spec draw_fast_hline(integer(), integer(), integer()) -> ok.
draw_fast_hline(_X, _Y, _W) ->
    throw(nif_error).

%% @doc Set color and draw a horizontal line.
%% Calls `start_write' and `end_write'.
-spec draw_fast_hline(integer(), integer(), integer(), rgb888()) -> ok.
draw_fast_hline(_X, _Y, _W, _Color) ->
    throw(nif_error).

%% @doc Fill a rectangle with current color.
%% Calls `start_write' and `end_write'.
-spec fill_rect(integer(), integer(), integer(), integer()) -> ok.
fill_rect(_X, _Y, _W, _H) ->
    throw(nif_error).

%% @doc Set color and fill a rectangle.
%% Calls `start_write' and `end_write'.
-spec fill_rect(integer(), integer(), integer(), integer(), rgb888()) -> ok.
fill_rect(_X, _Y, _W, _H, _Color) ->
    throw(nif_error).

%% @doc Draw a rectangle with current color.
%% Calls `start_write' and `end_write'.
-spec draw_rect(integer(), integer(), integer(), integer()) -> ok.
draw_rect(_X, _Y, _W, _H) ->
    throw(nif_error).

%% @doc Set color and draw a rectangle.
%% Calls `start_write' and `end_write'.
-spec draw_rect(integer(), integer(), integer(), integer(), rgb888()) -> ok.
draw_rect(_X, _Y, _W, _H, _Color) ->
    throw(nif_error).

%% @doc Draw a round rectangle with current color.
%% Calls `start_write' and `end_write'.
-spec draw_round_rect(integer(), integer(), integer(), integer(), integer()) -> ok.
draw_round_rect(_X, _Y, _W, _H, _R) ->
    throw(nif_error).

%% @doc Set color and draw a round rectangle.
%% Calls `start_write' and `end_write'.
-spec draw_round_rect(integer(), integer(), integer(), integer(), integer(), rgb888()) -> ok.
draw_round_rect(_X, _Y, _W, _H, _R, _Color) ->
    throw(nif_error).

%% @doc Fill a round rectangle with current color.
%% Calls `start_write' and `end_write'.
-spec fill_round_rect(integer(), integer(), integer(), integer(), integer()) -> ok.
fill_round_rect(_X, _Y, _W, _H, _R) ->
    throw(nif_error).

%% @doc Set color and fill a round rectangle.
%% Calls `start_write' and `end_write'.
-spec fill_round_rect(integer(), integer(), integer(), integer(), integer(), rgb888()) -> ok.
fill_round_rect(_X, _Y, _W, _H, _R, _Color) ->
    throw(nif_error).

%% @doc Draw a circle with current color.
%% Calls `start_write' and `end_write'.
-spec draw_circle(integer(), integer(), integer()) -> ok.
draw_circle(_X, _Y, _R) ->
    throw(nif_error).

%% @doc Set color and draw a circle.
%% Calls `start_write' and `end_write'.
-spec draw_circle(integer(), integer(), integer(), rgb888()) -> ok.
draw_circle(_X, _Y, _R, _Color) ->
    throw(nif_error).

%% @doc Fill a circle with current color.
%% Calls `start_write' and `end_write'.
-spec fill_circle(integer(), integer(), integer()) -> ok.
fill_circle(_X, _Y, _R) ->
    throw(nif_error).

%% @doc Set color and fill a circle.
%% Calls `start_write' and `end_write'.
-spec fill_circle(integer(), integer(), integer(), rgb888()) -> ok.
fill_circle(_X, _Y, _R, _Color) ->
    throw(nif_error).

%% @doc Draw an ellipse with current color.
%% Calls `start_write' and `end_write'.
-spec draw_ellipse(integer(), integer(), integer(), integer()) -> ok.
draw_ellipse(_X, _Y, _RX, _RY) ->
    throw(nif_error).

%% @doc Set color and draw an ellipse.
%% Calls `start_write' and `end_write'.
-spec draw_ellipse(integer(), integer(), integer(), integer(), rgb888()) -> ok.
draw_ellipse(_X, _Y, _RX, _RY, _Color) ->
    throw(nif_error).

%% @doc Fill an ellipse with current color.
%% Calls `start_write' and `end_write'.
-spec fill_ellipse(integer(), integer(), integer(), integer()) -> ok.
fill_ellipse(_X, _Y, _RX, _RY) ->
    throw(nif_error).

%% @doc Set color and fill an ellipse.
%% Calls `start_write' and `end_write'.
-spec fill_ellipse(integer(), integer(), integer(), integer(), rgb888()) -> ok.
fill_ellipse(_X, _Y, _RX, _RY, _Color) ->
    throw(nif_error).

%% @doc Draw a line with current color.
%% Calls `start_write' and `end_write'.
-spec draw_line(integer(), integer(), integer(), integer()) -> ok.
draw_line(_X0, _Y0, _X1, _Y1) ->
    throw(nif_error).

%% @doc Set color and draw a line.
%% Calls `start_write' and `end_write'.
-spec draw_line(integer(), integer(), integer(), integer(), rgb888()) -> ok.
draw_line(_X0, _Y0, _X1, _Y1, _Color) ->
    throw(nif_error).

%% @doc Draw a triangle with current color.
%% Calls `start_write' and `end_write'.
-spec draw_triangle(integer(), integer(), integer(), integer(), integer(), integer()) -> ok.
draw_triangle(_X0, _Y0, _X1, _Y1, _X2, _Y2) ->
    throw(nif_error).

%% @doc Set color and draw a triangle.
%% Calls `start_write' and `end_write'.
-spec draw_triangle(integer(), integer(), integer(), integer(), integer(), integer(), rgb888()) ->
    ok.
draw_triangle(_X0, _Y0, _X1, _Y1, _X2, _Y2, _Color) ->
    throw(nif_error).

%% @doc Fill a triangle with current color.
%% Calls `start_write' and `end_write'.
-spec fill_triangle(integer(), integer(), integer(), integer(), integer(), integer()) -> ok.
fill_triangle(_X0, _Y0, _X1, _Y1, _X2, _Y2) ->
    throw(nif_error).

%% @doc Set color and fill a triangle.
%% Calls `start_write' and `end_write'.
-spec fill_triangle(integer(), integer(), integer(), integer(), integer(), integer(), rgb888()) ->
    ok.
fill_triangle(_X0, _Y0, _X1, _Y1, _X2, _Y2, _Color) ->
    throw(nif_error).

%% @doc Draw a bezier with current color.
%% Calls `start_write' and `end_write'.
-spec draw_bezier(integer(), integer(), integer(), integer(), integer(), integer()) -> ok.
draw_bezier(_X0, _Y0, _X1, _Y1, _X2, _Y2) ->
    throw(nif_error).

%% @doc Set color and draw a bezier.
%% Calls `start_write' and `end_write'.
-spec draw_bezier(integer(), integer(), integer(), integer(), integer(), integer(), rgb888()) -> ok.
draw_bezier(_X0, _Y0, _X1, _Y1, _X2, _Y2, _Color) ->
    throw(nif_error).

%% @doc Draw a bezier with current color.
%% Calls `start_write' and `end_write'.
-spec draw_bezier(
    integer(), integer(), integer(), integer(), integer(), integer(), integer(), integer()
) -> ok.
draw_bezier(_X0, _Y0, _X1, _Y1, _X2, _Y2, _X3, _Y3) ->
    throw(nif_error).

%% @doc Set color and draw a bezier.
%% Calls `start_write' and `end_write'.
-spec draw_bezier(
    integer(), integer(), integer(), integer(), integer(), integer(), integer(), integer(), rgb888()
) -> ok.
draw_bezier(_X0, _Y0, _X1, _Y1, _X2, _Y2, _X3, _Y3, _Color) ->
    throw(nif_error).

%% @doc Fill the screen with current color
-spec fill_screen() -> ok.
fill_screen() ->
    throw(nif_error).

%% @doc Set current color and fill the screen
-spec fill_screen(rgb888()) -> ok.
fill_screen(_Color) ->
    throw(nif_error).

%% @doc Clear and fill the screen with base color
-spec clear() -> ok.
clear() ->
    throw(nif_error).

%% @doc Clear and fill the screen with provided color
-spec clear(rgb888()) -> ok.
clear(_Color) ->
    throw(nif_error).

%% @doc Return display width
-spec width() -> integer().
width() ->
    throw(nif_error).

%% @doc Return display height
-spec height() -> integer().
height() ->
    throw(nif_error).

%% @doc Wait until display is no longer busy
-spec wait_display() -> ok.
wait_display() ->
    throw(nif_error).

%% @doc Determine if display is busy
-spec display_busy() -> boolean().
display_busy() ->
    throw(nif_error).

%% @doc Set auto display
-spec set_auto_display(boolean()) -> ok.
set_auto_display(_Flag) ->
    throw(nif_error).

%% @doc Get the current rotation
-spec get_rotation() -> rotation().
get_rotation() ->
    throw(nif_error).

%% @doc Set the rotation of the main display
-spec set_rotation(rotation()) -> ok.
set_rotation(_Rotation) ->
    throw(nif_error).

%% @doc Set the clip rectangle
-spec set_clip_rect(integer(), integer(), integer(), integer()) -> ok.
set_clip_rect(_X, _Y, _W, _H) ->
    throw(nif_error).

%% @doc Get the clip rectangle
-spec get_clip_rect() -> {integer(), integer(), integer(), integer()}.
get_clip_rect() ->
    throw(nif_error).

%% @doc Clear the clip rectangle
-spec clear_clip_rect() -> ok.
clear_clip_rect() ->
    throw(nif_error).

%% @doc Set the scroll rectangle
-spec set_scroll_rect(integer(), integer(), integer(), integer()) -> ok.
set_scroll_rect(_X, _Y, _W, _H) ->
    throw(nif_error).

%% @doc Get the scroll rectangle
-spec get_scroll_rect() -> {integer(), integer(), integer(), integer()}.
get_scroll_rect() ->
    throw(nif_error).

%% @doc Clear the scroll rectangle
-spec clear_scroll_rect() -> ok.
clear_scroll_rect() ->
    throw(nif_error).

%% @doc Get the current (text) cursor
-spec get_cursor() -> {integer(), integer()}.
get_cursor() ->
    throw(nif_error).

%% @doc Set the (text) cursor
-spec set_cursor(integer(), integer()) -> ok.
set_cursor(_X, _Y) ->
    throw(nif_error).

%% @doc Set the text size
-spec set_text_size(number()) -> ok.
set_text_size(_TextSize) ->
    throw(nif_error).

%% @doc Set the text X and Y sizes
-spec set_text_size(number(), number()) -> ok.
set_text_size(_TextSizeX, _TextSizeY) ->
    throw(nif_error).

%% @doc Get the current font height, depending on the text size
-spec font_height() -> integer().
font_height() ->
    throw(nif_error).

%% @doc Get the current font width, depending on the text size
-spec font_width() -> integer().
font_width() ->
    throw(nif_error).

%% @doc Draw a string at a given coordinates for the left point
-spec draw_string(iodata(), integer(), integer()) -> ok.
draw_string(_String, _X, _Y) ->
    throw(nif_error).

%% @doc Draw a string at a given coordinates for the center point
-spec draw_center_string(iodata(), integer(), integer()) -> ok.
draw_center_string(_String, _X, _Y) ->
    throw(nif_error).

%% @doc Draw a string at a given coordinates for the right point
-spec draw_right_string(iodata(), integer(), integer()) -> ok.
draw_right_string(_String, _X, _Y) ->
    throw(nif_error).

%% @doc Print a string with a new line
-spec print(iodata()) -> ok.
print(_String) ->
    throw(nif_error).

%% @doc Print a new line
-spec println() -> ok.
println() ->
    throw(nif_error).

%% @doc Print a string with a new line
-spec println(iodata()) -> ok.
println(_String) ->
    throw(nif_error).
