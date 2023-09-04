/* SPDX-License-Identifier: MIT */
#include <sdkconfig.h>

#ifdef CONFIG_AVM_M5_DISPLAY_ENABLE

#include <stdlib.h>

#include "atomvm_m5.h"

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wpedantic"

#include <esp_attr.h>
#include <esp_log.h>
#include <esp_system.h>

#include <M5Unified.h>

#pragma GCC diagnostic pop

#include <context.h>
#include <defaultatoms.h>
#include <esp32_sys.h>
#include <interop.h>
#include <memory.h>
#include <nifs.h>
#include <term.h>
// #define ENABLE_TRACE
#include <trace.h>
#include <utils.h>

#include "atomvm_m5_nifs.h"

#define MODULE_PREFIX "m5_display:"

static term nif_display_set_epd_mode(Context* ctx, int argc, term argv[])
{
    UNUSED(argc);

    M5.Display.setEpdMode(
        epd_mode_t::epd_fastest); // fastest but very-low quality.
    return OK_ATOM;
}

M5_NIF_i_v(nif_display_set_brightness, Display, setBrightness)

M5_NIF_v_v(nif_display_sleep, Display, sleep)
M5_NIF_v_v(nif_display_wakeup, Display, wakeup)
M5_NIF_b_v(nif_display_power_save, Display, powerSave)
M5_NIF_v_v(nif_display_power_save_on, Display, powerSaveOn)
M5_NIF_v_v(nif_display_power_save_off, Display, powerSaveOff)

// Color is specifically handled.
// Functions taking colors currently only take rgb888 integers.
// set_color/1,3 is the exception and can be used for other formats.
static term nif_display_set_color(Context* ctx, int argc, term argv[])
{
    if (argc == 3) {
        VALIDATE_VALUE(argv[0], term_is_integer);
        VALIDATE_VALUE(argv[1], term_is_integer);
        VALIDATE_VALUE(argv[2], term_is_integer);
        int r = term_to_int(argv[0]);
        int g = term_to_int(argv[1]);
        int b = term_to_int(argv[2]);
        M5.Display.setColor(r, g, b);
    } else if (term_is_integer(argv[0])) {
        // We do RGB888 by default
        uint32_t rgb888 = term_to_int(argv[0]);
        M5.Display.setColor(rgb888);
    } else if (term_is_tuple(argv[0]) && term_get_tuple_arity(argv[0]) == 2 && term_is_atom(term_get_tuple_element(argv[0], 0))) {
        // Tagged value
        term tag = term_get_tuple_element(argv[0], 0);
        term val = term_get_tuple_element(argv[0], 1);
        if (globalcontext_is_term_equal_to_atom_string(ctx->global, tag, "\6rgb888") && term_is_integer(val)) {
            uint32_t rgb888 = term_to_int(val);
            M5.Display.setColor(rgb888);
        } else if (globalcontext_is_term_equal_to_atom_string(ctx->global, tag, "\6rgb565") && term_is_integer(val)) {
            int32_t rgb565 = term_to_int(val);
            M5.Display.setColor(rgb565);
        } else if (globalcontext_is_term_equal_to_atom_string(ctx->global, tag, "\3rgb") && term_is_tuple(val) && term_get_tuple_arity(val) == 3) {
            VALIDATE_VALUE(term_get_tuple_element(val, 0), term_is_integer);
            VALIDATE_VALUE(term_get_tuple_element(val, 1), term_is_integer);
            VALIDATE_VALUE(term_get_tuple_element(val, 2), term_is_integer);
            int r = term_to_int(term_get_tuple_element(val, 0));
            int g = term_to_int(term_get_tuple_element(val, 1));
            int b = term_to_int(term_get_tuple_element(val, 2));
            M5.Display.setColor(r, g, b);
        } else {
            RAISE_ERROR(BADARG_ATOM);
        }
    } else {
        RAISE_ERROR(BADARG_ATOM);
    }
    return OK_ATOM;
}

M5_NIF_i_v(nif_display_set_raw_color, Display, setRawColor)
M5_NIF_v_i(nif_display_get_raw_color, Display, getRawColor)
M5_NIF_u_v(nif_display_set_base_color, Display, setBaseColor)
M5_NIF_v_i(nif_display_get_base_color, Display, getBaseColor)

M5_NIF_v_v(nif_display_start_write, Display, startWrite)
M5_NIF_v_v(nif_display_end_write, Display, endWrite)

M5_NIF_i2_i2u_v(nif_display_write_pixel, Display, writePixel)
M5_NIF_i3_i3u_v(nif_display_write_fast_vline, Display, writeFastVLine)
M5_NIF_i3_i3u_v(nif_display_write_fast_hline, Display, writeFastHLine)
M5_NIF_i4_i4u_v(nif_display_write_fill_rect, Display, writeFillRect)
M5_NIF_i4_i4u_v(nif_display_write_fill_rect_preclipped, Display, writeFillRectPreclipped)
M5_NIF_i2_v(nif_display_write_color, Display, writeColor)
M5_NIF_i2_v(nif_display_push_block, Display, pushBlock)
M5_NIF_i2_i2u_v(nif_display_draw_pixel, Display, drawPixel)
M5_NIF_i3_i3u_v(nif_display_draw_fast_vline, Display, drawFastVLine)
M5_NIF_i3_i3u_v(nif_display_draw_fast_hline, Display, drawFastHLine)
M5_NIF_i4_i4u_v(nif_display_fill_rect, Display, fillRect)
M5_NIF_i4_i4u_v(nif_display_draw_rect, Display, drawRect)
M5_NIF_i5_i5u_v(nif_display_draw_round_rect, Display, drawRoundRect)
M5_NIF_i5_i5u_v(nif_display_fill_round_rect, Display, fillRoundRect)
M5_NIF_i3_i3u_v(nif_display_draw_circle, Display, drawCircle)
M5_NIF_i3_i3u_v(nif_display_fill_circle, Display, fillCircle)
M5_NIF_i4_i4u_v(nif_display_draw_ellipse, Display, drawEllipse)
M5_NIF_i4_i4u_v(nif_display_fill_ellipse, Display, fillEllipse)
M5_NIF_i4_i4u_v(nif_display_draw_line, Display, drawLine)
M5_NIF_i6_i6u_v(nif_display_draw_triangle, Display, drawTriangle)
M5_NIF_i6_i6u_v(nif_display_fill_triangle, Display, fillTriangle)

static term nif_display_draw_bezier(Context* ctx, int argc, term argv[])
{
    for (int i = 0; i < argc; i++) {
        VALIDATE_VALUE(argv[i], term_is_integer);
    }
    switch (argc) {
    case 6:
        M5.Display.drawBezier(term_to_int(argv[0]), term_to_int(argv[1]),
            term_to_int(argv[2]), term_to_int(argv[3]),
            term_to_int(argv[4]), term_to_int(argv[5]));
        break;
    case 7:
        M5.Display.drawBezier(term_to_int(argv[0]), term_to_int(argv[1]),
            term_to_int(argv[2]), term_to_int(argv[3]),
            term_to_int(argv[4]), term_to_int(argv[5]),
            (uint32_t)term_to_int(argv[6]));
        break;
    case 8:
        M5.Display.drawBezier(term_to_int(argv[0]), term_to_int(argv[1]),
            term_to_int(argv[2]), term_to_int(argv[3]),
            term_to_int(argv[4]), term_to_int(argv[5]),
            term_to_int(argv[6]), term_to_int(argv[7]));
        break;
    case 9:
        M5.Display.drawBezier(
            term_to_int(argv[0]), term_to_int(argv[1]), term_to_int(argv[2]),
            term_to_int(argv[3]), term_to_int(argv[4]), term_to_int(argv[5]),
            term_to_int(argv[6]), term_to_int(argv[7]), (uint32_t)term_to_int(argv[8]));
        break;
    }
    return OK_ATOM;
}

M5_NIF_v_u_v(nif_display_fill_screen, Display, fillScreen)
M5_NIF_v_u_v(nif_display_clear, Display, clear)
M5_NIF_v_i(nif_display_width, Display, width)
M5_NIF_v_i(nif_display_height, Display, height)
M5_NIF_v_v(nif_display_wait_display, Display, waitDisplay)
M5_NIF_v_b(nif_display_display_busy, Display, displayBusy)
M5_NIF_b_v(nif_display_set_auto_display, Display, setAutoDisplay)
M5_NIF_v_i(nif_display_get_rotation, Display, getRotation)
M5_NIF_i_v(nif_display_set_rotation, Display, setRotation)
M5_NIF_i4_v(nif_display_set_clip_rect, Display, setClipRect)
M5_NIF_i32o4_v(nif_display_get_clip_rect, Display, getClipRect)
M5_NIF_v_v(nif_display_clear_clip_rect, Display, clearClipRect)
M5_NIF_i4_v(nif_display_set_scroll_rect, Display, setScrollRect)
M5_NIF_i32o4_v(nif_display_get_scroll_rect, Display, getScrollRect)
M5_NIF_v_v(nif_display_clear_scroll_rect, Display, clearScrollRect)

static term nif_display_get_cursor(Context* ctx, int argc, term argv[])
{
    UNUSED(argc);
    UNUSED(argv);
    if (UNLIKELY(memory_ensure_free(ctx, 3) != MEMORY_GC_OK)) {
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    }
    term result = term_alloc_tuple(2, &ctx->heap);
    term_put_tuple_element(result, 0, term_from_int32(M5.Display.getCursorX()));
    term_put_tuple_element(result, 1, term_from_int32(M5.Display.getCursorY()));

    return result;
}

M5_NIF_i2_v(nif_display_set_cursor, Display, setCursor)
M5_NIF_f_f2_v(nif_display_set_text_size, Display, setTextSize)
M5_NIF_v_i(nif_display_font_height, Display, fontHeight)
M5_NIF_v_i(nif_display_font_width, Display, fontWidth)
M5_NIF_si2_i(nif_draw_string, Display, drawString)
M5_NIF_si2_i(nif_draw_center_string, Display, drawCenterString)
M5_NIF_si2_i(nif_draw_right_string, Display, drawRightString)

static term nif_display_print(Context* ctx, int argc, term argv[])
{
    UNUSED(argc);

    size_t bin_size;
    switch (interop_iolist_size(argv[0], &bin_size)) {
    case InteropOk:
        break;
    case InteropMemoryAllocFail:
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    case InteropBadArg:
        RAISE_ERROR(BADARG_ATOM);
    }

    char* bin_buf = (char*)malloc(bin_size);
    if (IS_NULL_PTR(bin_buf)) {
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    }

    switch (interop_write_iolist(argv[0], bin_buf)) {
    case InteropOk:
        break;
    case InteropMemoryAllocFail:
        free(bin_buf);
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    case InteropBadArg:
        free(bin_buf);
        RAISE_ERROR(BADARG_ATOM);
    }

    size_t r = M5.Display.write(bin_buf, bin_size);
    free(bin_buf);
    return term_from_int(r);
}

static term nif_display_println(Context* ctx, int argc, term argv[])
{
    size_t r = 0;
    if (argc == 1) {
        term t = nif_display_print(ctx, argc, argv);
        if (UNLIKELY(!term_is_integer(t))) {
            return t;
        }
        r = term_to_int(t);
    }
    r += M5.Display.println();
    return term_from_int(r);
}

static constexpr std::array<std::pair<const char*, const struct Nif>, 88>
    NIFS = { {
        { "set_epd_mode/1", { { NIFFunctionType }, nif_display_set_epd_mode } },
        { "set_brightness/1", { { NIFFunctionType }, nif_display_set_brightness } },
        { "sleep/0", { { NIFFunctionType }, nif_display_sleep } },
        { "wakeup/0", { { NIFFunctionType }, nif_display_wakeup } },
        { "power_save/1", { { NIFFunctionType }, nif_display_power_save } },
        { "power_save_on/0", { { NIFFunctionType }, nif_display_power_save_on } },
        { "power_save_off/0", { { NIFFunctionType }, nif_display_power_save_off } },

        { "set_color/1", { { NIFFunctionType }, nif_display_set_color } },
        { "set_color/3", { { NIFFunctionType }, nif_display_set_color } },
        { "set_raw_color/1", { { NIFFunctionType }, nif_display_set_raw_color } },
        { "get_raw_color/0", { { NIFFunctionType }, nif_display_get_raw_color } },
        { "set_base_color/1", { { NIFFunctionType }, nif_display_set_base_color } },
        { "get_base_color/0", { { NIFFunctionType }, nif_display_get_base_color } },

        { "start_write/0", { { NIFFunctionType }, nif_display_start_write } },
        { "end_write/0", { { NIFFunctionType }, nif_display_end_write } },

        { "write_pixel/2", { { NIFFunctionType }, nif_display_write_pixel } },
        { "write_pixel/3", { { NIFFunctionType }, nif_display_write_pixel } },
        { "write_fast_vline/3",
            { { NIFFunctionType }, nif_display_write_fast_vline } },
        { "write_fast_vline/4",
            { { NIFFunctionType }, nif_display_write_fast_vline } },
        { "write_fast_hline/3",
            { { NIFFunctionType }, nif_display_write_fast_hline } },
        { "write_fast_hline/4",
            { { NIFFunctionType }, nif_display_write_fast_hline } },
        { "write_fill_rect/4", { { NIFFunctionType }, nif_display_write_fill_rect } },
        { "write_fill_rect/5", { { NIFFunctionType }, nif_display_write_fill_rect } },
        { "write_fill_rect_preclipped/4",
            { { NIFFunctionType }, nif_display_write_fill_rect_preclipped } },
        { "write_fill_rect_preclipped/5",
            { { NIFFunctionType }, nif_display_write_fill_rect_preclipped } },
        { "write_color/2", { { NIFFunctionType }, nif_display_write_color } },
        { "push_block/2", { { NIFFunctionType }, nif_display_push_block } },
        { "draw_pixel/2", { { NIFFunctionType }, nif_display_draw_pixel } },
        { "draw_pixel/3", { { NIFFunctionType }, nif_display_draw_pixel } },
        { "draw_fast_vline/3", { { NIFFunctionType }, nif_display_draw_fast_vline } },
        { "draw_fast_vline/4", { { NIFFunctionType }, nif_display_draw_fast_vline } },
        { "draw_fast_hline/3", { { NIFFunctionType }, nif_display_draw_fast_hline } },
        { "draw_fast_hline/4", { { NIFFunctionType }, nif_display_draw_fast_hline } },
        { "fill_rect/4", { { NIFFunctionType }, nif_display_fill_rect } },
        { "fill_rect/5", { { NIFFunctionType }, nif_display_fill_rect } },
        { "draw_rect/4", { { NIFFunctionType }, nif_display_draw_rect } },
        { "draw_rect/5", { { NIFFunctionType }, nif_display_draw_rect } },
        { "draw_round_rect/5", { { NIFFunctionType }, nif_display_draw_round_rect } },
        { "draw_round_rect/6", { { NIFFunctionType }, nif_display_draw_round_rect } },
        { "fill_round_rect/5", { { NIFFunctionType }, nif_display_fill_round_rect } },
        { "fill_round_rect/6", { { NIFFunctionType }, nif_display_fill_round_rect } },
        { "draw_circle/3", { { NIFFunctionType }, nif_display_draw_circle } },
        { "draw_circle/4", { { NIFFunctionType }, nif_display_draw_circle } },
        { "fill_circle/3", { { NIFFunctionType }, nif_display_fill_circle } },
        { "fill_circle/4", { { NIFFunctionType }, nif_display_fill_circle } },
        { "draw_ellipse/4", { { NIFFunctionType }, nif_display_draw_ellipse } },
        { "draw_ellipse/5", { { NIFFunctionType }, nif_display_draw_ellipse } },
        { "fill_ellipse/4", { { NIFFunctionType }, nif_display_fill_ellipse } },
        { "fill_ellipse/5", { { NIFFunctionType }, nif_display_fill_ellipse } },
        { "draw_line/4", { { NIFFunctionType }, nif_display_draw_line } },
        { "draw_line/5", { { NIFFunctionType }, nif_display_draw_line } },
        { "draw_triangle/6", { { NIFFunctionType }, nif_display_draw_triangle } },
        { "draw_triangle/7", { { NIFFunctionType }, nif_display_draw_triangle } },
        { "fill_triangle/6", { { NIFFunctionType }, nif_display_fill_triangle } },
        { "fill_triangle/7", { { NIFFunctionType }, nif_display_fill_triangle } },
        { "draw_bezier/6", { { NIFFunctionType }, nif_display_draw_bezier } },
        { "draw_bezier/7", { { NIFFunctionType }, nif_display_draw_bezier } },
        { "draw_bezier/8", { { NIFFunctionType }, nif_display_draw_bezier } },
        { "draw_bezier/9", { { NIFFunctionType }, nif_display_draw_bezier } },

        { "fill_screen/0", { { NIFFunctionType }, nif_display_fill_screen } },
        { "fill_screen/1", { { NIFFunctionType }, nif_display_fill_screen } },
        { "clear/0", { { NIFFunctionType }, nif_display_clear } },
        { "clear/1", { { NIFFunctionType }, nif_display_clear } },

        { "width/0", { { NIFFunctionType }, nif_display_width } },
        { "height/0", { { NIFFunctionType }, nif_display_height } },

        { "wait_display/0", { { NIFFunctionType }, nif_display_wait_display } },
        { "display_busy/0", { { NIFFunctionType }, nif_display_display_busy } },
        { "set_auto_display/1", { { NIFFunctionType }, nif_display_set_auto_display } },

        { "get_rotation/0", { { NIFFunctionType }, nif_display_get_rotation } },
        { "set_rotation/1", { { NIFFunctionType }, nif_display_set_rotation } },

        { "set_clip_rect/4", { { NIFFunctionType }, nif_display_set_clip_rect } },
        { "get_clip_rect/0", { { NIFFunctionType }, nif_display_get_clip_rect } },
        { "clear_clip_rect/0", { { NIFFunctionType }, nif_display_clear_clip_rect } },
        { "set_scroll_rect/4", { { NIFFunctionType }, nif_display_set_scroll_rect } },
        { "get_scroll_rect/0", { { NIFFunctionType }, nif_display_get_scroll_rect } },
        { "clear_scroll_rect/0", { { NIFFunctionType }, nif_display_clear_scroll_rect } },

        { "get_cursor/0", { { NIFFunctionType }, nif_display_get_cursor } },
        { "set_cursor/2", { { NIFFunctionType }, nif_display_set_cursor } },
        { "set_text_size/1", { { NIFFunctionType }, nif_display_set_text_size } },
        { "set_text_size/2", { { NIFFunctionType }, nif_display_set_text_size } },

        { "font_height/0", { { NIFFunctionType }, nif_display_font_height } },
        { "font_width/0", { { NIFFunctionType }, nif_display_font_width } },

        { "draw_string/3", { { NIFFunctionType }, nif_draw_string } },
        { "draw_center_string/3", { { NIFFunctionType }, nif_draw_center_string } },
        { "draw_right_string/3", { { NIFFunctionType }, nif_draw_right_string } },

        { "print/1", { { NIFFunctionType }, nif_display_print } },
        { "println/1", { { NIFFunctionType }, nif_display_println } },
        { "println/0", { { NIFFunctionType }, nif_display_println } },
    } };

//
// Component Nif Entrypoints
//

static const struct Nif* get_nif(const char* nifname)
{
    if (memcmp(nifname, MODULE_PREFIX, strlen(MODULE_PREFIX))) {
        return NULL;
    }
    for (const auto& nif : NIFS) {
        if (strcmp(nif.first, nifname + strlen(MODULE_PREFIX)) == 0) {
            return &nif.second;
        }
    }
    return NULL;
}

REGISTER_NIF_COLLECTION(m5_display, NULL, NULL, get_nif)

#endif
