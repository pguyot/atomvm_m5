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

M5_NIF_v_v(nif_display_sleep, Display, sleep)
M5_NIF_v_v(nif_display_start_write, Display, startWrite)
M5_NIF_v_v(nif_display_end_write, Display, endWrite)

M5_NIF_i2_i3_v(nif_display_write_pixel, Display, writePixel)
M5_NIF_i3_i4_v(nif_display_write_fast_vline, Display, writeFastVLine)
M5_NIF_i3_i4_v(nif_display_write_fast_hline, Display, writeFastHLine)
M5_NIF_i4_i5_v(nif_display_write_fill_rect, Display, writeFillRect)
M5_NIF_i4_i5_v(nif_display_write_fill_rect_preclipped, Display, writeFillRectPreclipped)
M5_NIF_i2_v(nif_display_write_color, Display, writeColor)
M5_NIF_i2_v(nif_display_push_block, Display, pushBlock)
M5_NIF_i2_i3_v(nif_display_draw_pixel, Display, drawPixel)
M5_NIF_i3_i4_v(nif_display_draw_fast_vline, Display, drawFastVLine)
M5_NIF_i3_i4_v(nif_display_draw_fast_hline, Display, drawFastHLine)
M5_NIF_i4_i5_v(nif_display_fill_rect, Display, fillRect)
M5_NIF_i4_i5_v(nif_display_draw_rect, Display, drawRect)
M5_NIF_i5_i6_v(nif_display_draw_round_rect, Display, drawRoundRect)
M5_NIF_i5_i6_v(nif_display_fill_round_rect, Display, fillRoundRect)
M5_NIF_i3_i4_v(nif_display_draw_circle, Display, drawCircle)
M5_NIF_i3_i4_v(nif_display_fill_circle, Display, fillCircle)
M5_NIF_i4_i5_v(nif_display_draw_ellipse, Display, drawEllipse)
M5_NIF_i4_i5_v(nif_display_fill_ellipse, Display, fillEllipse)
M5_NIF_i4_i5_v(nif_display_draw_line, Display, drawLine)
M5_NIF_i6_i7_v(nif_display_draw_triangle, Display, drawTriangle)
M5_NIF_i6_i7_v(nif_display_fill_triangle, Display, fillTriangle)

static term nif_display_draw_bezier(Context* ctx, int argc, term argv[])
{
    for (int i = 0; i < argc; i++) {
        VALIDATE_VALUE(argv[i], term_is_integer);
    }
    switch (argc) {
    case 6:
        M5.Display.drawBezier(term_to_int32(argv[0]), term_to_int32(argv[1]),
            term_to_int32(argv[2]), term_to_int32(argv[3]),
            term_to_int32(argv[4]), term_to_int32(argv[5]));
        break;
    case 7:
        M5.Display.drawBezier(term_to_int32(argv[0]), term_to_int32(argv[1]),
            term_to_int32(argv[2]), term_to_int32(argv[3]),
            term_to_int32(argv[4]), term_to_int32(argv[5]),
            term_to_int32(argv[6]));
        break;
    case 8:
        M5.Display.drawBezier(term_to_int32(argv[0]), term_to_int32(argv[1]),
            term_to_int32(argv[2]), term_to_int32(argv[3]),
            term_to_int32(argv[4]), term_to_int32(argv[5]),
            term_to_int32(argv[6]), term_to_int32(argv[7]));
        break;
    case 9:
        M5.Display.drawBezier(
            term_to_int32(argv[0]), term_to_int32(argv[1]), term_to_int32(argv[2]),
            term_to_int32(argv[3]), term_to_int32(argv[4]), term_to_int32(argv[5]),
            term_to_int32(argv[6]), term_to_int32(argv[7]), term_to_int32(argv[8]));
        break;
    }
    return OK_ATOM;
}

M5_NIF_v_i(nif_display_width, Display, width)
M5_NIF_v_i(nif_display_height, Display, height)
M5_NIF_v_v(nif_display_wait_display, Display, waitDisplay)
M5_NIF_v_i(nif_display_get_rotation, Display, getRotation)
M5_NIF_i_v(nif_display_set_rotation, Display, setRotation)

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
M5_NIF_i_v(nif_display_set_text_size, Display, setTextSize)

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

static constexpr std::array<std::pair<const char*, const struct Nif>, 59>
    NIFS = { {
        { "set_epd_mode/1", { { NIFFunctionType }, nif_display_set_epd_mode } },
        { "sleep/0", { { NIFFunctionType }, nif_display_sleep } },

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

        { "width/0", { { NIFFunctionType }, nif_display_width } },
        { "height/0", { { NIFFunctionType }, nif_display_height } },

        { "wait_display/0", { { NIFFunctionType }, nif_display_wait_display } },

        { "get_rotation/0", { { NIFFunctionType }, nif_display_get_rotation } },
        { "set_rotation/1", { { NIFFunctionType }, nif_display_set_rotation } },

        { "get_cursor/0", { { NIFFunctionType }, nif_display_get_cursor } },
        { "set_cursor/2", { { NIFFunctionType }, nif_display_set_cursor } },
        { "set_text_size/1", { { NIFFunctionType }, nif_display_set_text_size } },

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
