/* SPDX-License-Identifier: MIT */
#include <sdkconfig.h>

#ifdef CONFIG_AVM_M5_ENABLE

#include <stdlib.h>

#include <atomvm_m5.h>
#include <esp_log.h>
#include <esp_attr.h>
#include <context.h>
#include <defaultatoms.h>
#include <interop.h>
#include <nifs.h>
#include <term.h>
#include <memory.h>
#include <esp_system.h>

#include "esp32_sys.h"

//#define ENABLE_TRACE
#include "trace.h"

#include <M5Unified.h>

static term nif_begin(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);
    auto cfg = M5.config();

#if defined ( ARDUINO )
    cfg.serial_baudrate = 115200;   // default=115200. if "Serial" is not needed, set it to 0.
#endif
    cfg.clear_display = true;  // default=true. clear the screen when begin.
    cfg.output_power  = true;  // default=true. use external port 5V output.
    cfg.internal_imu  = true;  // default=true. use internal IMU.
    cfg.internal_rtc  = true;  // default=true. use internal RTC.
    cfg.internal_spk  = true;  // default=true. use internal speaker.
    cfg.internal_mic  = true;  // default=true. use internal microphone.
    cfg.external_imu  = true;  // default=false. use Unit Accel & Gyro.
    cfg.external_rtc  = true;  // default=false. use Unit RTC.
    cfg.external_spk  = false; // default=false. use SPK_HAT / ATOMIC_SPK
    //cfg.external_spk_detail.omit_atomic_spk = true; // omit ATOMIC SPK
    //cfg.external_spk_detail.omit_spk_hat    = true; // omit SPK HAT
    cfg.led_brightness = 64;   // default= 0. system LED brightness (0=off / 255=max) (※ not NeoPixel)


    M5.begin(cfg);

    return OK_ATOM;
}

static term nif_get_board(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);

    switch (M5.getBoard())
    {
#if defined (CONFIG_IDF_TARGET_ESP32C3)
        case m5::board_t::board_M5StampC3:
            return globalcontext_make_atom(ctx, ATOM_STR("\x8", "stamp_c3"));
        case m5::board_t::board_M5StampC3U:
            return globalcontext_make_atom(ctx->global, ATOM_STR("\x9", "stamp_c3u"));
#else
        case m5::board_t::board_M5Stack:
            return globalcontext_make_atom(ctx->global, ATOM_STR("\x5", "stack"));
        case m5::board_t::board_M5StackCore2:
            return globalcontext_make_atom(ctx->global, ATOM_STR("\xB", "stack_core2"));
        case m5::board_t::board_M5StickC:
            return globalcontext_make_atom(ctx->global, ATOM_STR("\x7", "stick_c"));
        case m5::board_t::board_M5StickCPlus:
            return globalcontext_make_atom(ctx->global, ATOM_STR("\xB", "stick_cplus"));
        case m5::board_t::board_M5StackCoreInk:
            return globalcontext_make_atom(ctx->global, ATOM_STR("\x8", "core_ink"));
        case m5::board_t::board_M5Paper:
            return globalcontext_make_atom(ctx->global, ATOM_STR("\x5", "paper"));
        case m5::board_t::board_M5Tough:
            return globalcontext_make_atom(ctx->global, ATOM_STR("\x5", "tough"));
        case m5::board_t::board_M5Station:
            return globalcontext_make_atom(ctx->global, ATOM_STR("\x7", "station"));
        case m5::board_t::board_M5Atom:
            return globalcontext_make_atom(ctx->global, ATOM_STR("\x4", "atom"));
        case m5::board_t::board_M5AtomPsram:
            return globalcontext_make_atom(ctx->global, ATOM_STR("\xA", "atom_psram"));
        case m5::board_t::board_M5AtomU:
            return globalcontext_make_atom(ctx->global, ATOM_STR("\x6", "atom_u"));
        case m5::board_t::board_M5TimerCam:
            return globalcontext_make_atom(ctx->global, ATOM_STR("\xC", "timer_camera"));
        case m5::board_t::board_M5StampPico:
            return globalcontext_make_atom(ctx->global, ATOM_STR("\xA", "stamp_pico"));
#endif
        default:
            return UNDEFINED_ATOM;
    }
}

static term nif_update(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);
    UNUSED(argv);

    M5.update();
    return OK_ATOM;
}

static term nif_display_set_epd_mode(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);

    M5.Display.setEpdMode(epd_mode_t::epd_fastest); // fastest but very-low quality.
    return OK_ATOM;
}

static term nif_display_height(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);

    return term_from_int32(M5.Display.height());
}

static term nif_display_width(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);

    return term_from_int32(M5.Display.width());
}

static term nif_display_get_rotation(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);

    return term_from_int32(M5.Display.getRotation());
}

static term nif_display_set_rotation(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);
    VALIDATE_VALUE(argv[0], term_is_integer);

    M5.Display.setRotation(term_to_int32(argv[0]));
    return OK_ATOM;
}

static term nif_display_set_text_size(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);
    VALIDATE_VALUE(argv[0], term_is_integer);

    M5.Display.setTextSize(term_to_int32(argv[0]));
    return OK_ATOM;
}

static term nif_display_start_write(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);
    UNUSED(argv);

    M5.Display.startWrite();

    return OK_ATOM;
}

static term nif_display_end_write(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);
    UNUSED(argv);

    M5.Display.endWrite();

    return OK_ATOM;
}

static term nif_display_fill_rect(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);
    VALIDATE_VALUE(argv[0], term_is_integer);
    VALIDATE_VALUE(argv[1], term_is_integer);
    VALIDATE_VALUE(argv[2], term_is_integer);
    VALIDATE_VALUE(argv[3], term_is_integer);
    VALIDATE_VALUE(argv[4], term_is_integer);

    M5.Display.fillRect(
        term_to_int32(argv[0]),
        term_to_int32(argv[1]),
        term_to_int32(argv[2]),
        term_to_int32(argv[3]),
        term_to_int32(argv[4])
    );

    return OK_ATOM;
}

static term nif_display_print(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);
    VALIDATE_VALUE(argv[0], term_is_binary);
    unsigned long binLen = term_binary_size(argv[0]);
    const char* bin = term_binary_data(argv[0]);
    size_t r = M5.Display.write(bin, binLen);
    return term_from_int(r);
}

static term nif_display_println(Context *ctx, int argc, term argv[])
{
    size_t r = 0;
    if (argc == 1) {
        VALIDATE_VALUE(argv[0], term_is_binary);
        unsigned long binLen = term_binary_size(argv[0]);
        const char* bin = term_binary_data(argv[0]);
        r = M5.Display.write(bin, binLen);
    }
    r += M5.Display.println();
    return term_from_int(r);
}

static term nif_power_timer_sleep(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);
    VALIDATE_VALUE(argv[0], term_is_integer);
    M5.Power.timerSleep(term_to_int32(argv[0]));
    return OK_ATOM;
}

static term nif_get_battery_level(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);
    UNUSED(argv);
    int32_t level = M5.Power.getBatteryLevel();
    return term_from_int(level);
}

constexpr std::array<std::pair<const char*, const struct Nif>, 17> M5_NIFS = {{
    {"m5:begin_/1", { { NIFFunctionType }, nif_begin }},
    {"m5:get_board/0", { { NIFFunctionType }, nif_get_board }},
    {"m5:update/0", { { NIFFunctionType }, nif_update }},

    {"m5_display:set_epd_mode/1", { { NIFFunctionType }, nif_display_set_epd_mode }},
    {"m5_display:height/0", { { NIFFunctionType }, nif_display_height }},
    {"m5_display:width/0", { { NIFFunctionType }, nif_display_width }},
    {"m5_display:set_rotation/1", { { NIFFunctionType }, nif_display_set_rotation }},
    {"m5_display:get_rotation/0", { { NIFFunctionType }, nif_display_get_rotation }},
    {"m5_display:set_text_size/1", { { NIFFunctionType }, nif_display_set_text_size }},
    {"m5_display:start_write/0", { { NIFFunctionType }, nif_display_start_write }},
    {"m5_display:end_write/0", { { NIFFunctionType }, nif_display_end_write }},
    {"m5_display:fill_rect/5", { { NIFFunctionType }, nif_display_fill_rect }},
    {"m5_display:print/1", { { NIFFunctionType }, nif_display_print }},
    {"m5_display:println/1", { { NIFFunctionType }, nif_display_println }},
    {"m5_display:println/0", { { NIFFunctionType }, nif_display_println }},

    {"m5_power:timer_sleep/1", { { NIFFunctionType }, nif_power_timer_sleep }},
    {"m5_power:get_battery_level/0", { { NIFFunctionType }, nif_get_battery_level }},
}};

//
// Component Nif Entrypoints
//

void m5_init(GlobalContext *global)
{
    // no-op
}

const struct Nif *m5_get_nif(const char *nifname)
{
    for (const auto& nif : M5_NIFS)
    {
        if (strcmp(nif.first, nifname) == 0) {
            return &nif.second;
        }
    }
    return NULL;
}

REGISTER_NIF_COLLECTION(m5, m5_init, m5_get_nif)

#endif
