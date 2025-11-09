/* SPDX-License-Identifier: MIT */
#include <sdkconfig.h>

#ifdef CONFIG_AVM_M5_ENABLE

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

#define MODULE_PREFIX "m5:"

#define MAKE_ATOM(ctx, len, str) globalcontext_make_atom(ctx->global, ATOM_STR(len, str))

static term nif_begin(Context* ctx, int argc, term argv[])
{
    UNUSED(argc);
    VALIDATE_VALUE(argv[0], term_is_list);
    auto cfg = M5.config();
    term opts = argv[0];

#if defined(ARDUINO)
    cfg.serial_baudrate = term_to_int32(interop_proplist_get_value_default(opts, MAKE_ATOM(ctx, "\xF", "serial_baudrate"), term_from_int(115200)));
#endif
    cfg.clear_display = interop_proplist_get_value_default(opts, MAKE_ATOM(ctx, "\xD", "clear_display"), TRUE_ATOM) == TRUE_ATOM;
    cfg.output_power = interop_proplist_get_value_default(opts, MAKE_ATOM(ctx, "\xC", "output_power"), TRUE_ATOM) == TRUE_ATOM;
    cfg.internal_imu = interop_proplist_get_value_default(opts, MAKE_ATOM(ctx, "\xC", "internal_imu"), TRUE_ATOM) == TRUE_ATOM;
    cfg.internal_rtc = interop_proplist_get_value_default(opts, MAKE_ATOM(ctx, "\xC", "internal_rtc"), TRUE_ATOM) == TRUE_ATOM;
    cfg.internal_spk = interop_proplist_get_value_default(opts, MAKE_ATOM(ctx, "\xC", "internal_spk"), TRUE_ATOM) == TRUE_ATOM;
    cfg.internal_mic = interop_proplist_get_value_default(opts, MAKE_ATOM(ctx, "\xC", "internal_mic"), TRUE_ATOM) == TRUE_ATOM;
    cfg.external_imu = interop_proplist_get_value_default(opts, MAKE_ATOM(ctx, "\xC", "external_imu"), FALSE_ATOM) == TRUE_ATOM;
    cfg.external_rtc = interop_proplist_get_value_default(opts, MAKE_ATOM(ctx, "\xC", "external_rtc"), FALSE_ATOM) == TRUE_ATOM;
    cfg.external_spk = interop_proplist_get_value_default(opts, MAKE_ATOM(ctx, "\xC", "external_spk"), FALSE_ATOM) == TRUE_ATOM;
    cfg.led_brightness = term_to_int32(interop_proplist_get_value_default(opts, MAKE_ATOM(ctx, "\xF", "led_brightness"), term_from_int(64)));

    M5.begin(cfg);

    return OK_ATOM;
}

static term nif_get_board(Context* ctx, int argc, term argv[])
{
    UNUSED(argc);

    switch (M5.getBoard()) {
#if defined(CONFIG_IDF_TARGET_ESP32S3)
    case m5::board_t::board_M5StackCoreS3:
        return MAKE_ATOM(ctx, "\x8", "stamp_core_s3");
    case m5::board_t::board_M5StampS3:
        return MAKE_ATOM(ctx, "\x9", "stamp_s3");
    case m5::board_t::board_M5AtomS3U:
        return MAKE_ATOM(ctx, "\x9", "atom_s3u");
    case m5::board_t::board_M5AtomS3Lite:
        return MAKE_ATOM(ctx, "\x9", "atom_s3lite");
    case m5::board_t::board_M5AtomS3:
        return MAKE_ATOM(ctx, "\x9", "atom_s3");
    case m5::board_t::board_M5Cardputer:
        return MAKE_ATOM(ctx, "\x9", "cardputer");
#elif defined(CONFIG_IDF_TARGET_ESP32C3)
    case m5::board_t::board_M5StampC3:
        return MAKE_ATOM(ctx, "\x8", "stamp_c3");
    case m5::board_t::board_M5StampC3U:
        return MAKE_ATOM(ctx, "\x9", "stamp_c3u");
#else
    case m5::board_t::board_M5Stack:
        return MAKE_ATOM(ctx, "\x5", "stack");
    case m5::board_t::board_M5StackCore2:
        return MAKE_ATOM(ctx, "\xB", "stack_core2");
    case m5::board_t::board_M5StickC:
        return MAKE_ATOM(ctx, "\x7", "stick_c");
    case m5::board_t::board_M5StickCPlus:
        return MAKE_ATOM(ctx, "\xB", "stick_cplus");
    case m5::board_t::board_M5StackCoreInk:
        return MAKE_ATOM(ctx, "\x8", "core_ink");
    case m5::board_t::board_M5Paper:
        return MAKE_ATOM(ctx, "\x5", "paper");
    case m5::board_t::board_M5Tough:
        return MAKE_ATOM(ctx, "\x5", "tough");
    case m5::board_t::board_M5Station:
        return MAKE_ATOM(ctx, "\x7", "station");
    case m5::board_t::board_M5Atom:
        return MAKE_ATOM(ctx, "\x4", "atom");
    case m5::board_t::board_M5AtomPsram:
        return MAKE_ATOM(ctx, "\xA", "atom_psram");
    case m5::board_t::board_M5AtomU:
        return MAKE_ATOM(ctx, "\x6", "atom_u");
    case m5::board_t::board_M5TimerCam:
        return MAKE_ATOM(ctx, "\xC", "timer_camera");
    case m5::board_t::board_M5StampPico:
        return MAKE_ATOM(ctx, "\xA", "stamp_pico");
#endif
    default:
        return UNDEFINED_ATOM;
    }
}

static term nif_update(Context* ctx, int argc, term argv[])
{
    UNUSED(argc);
    UNUSED(argv);

    M5.update();
    return OK_ATOM;
}

static constexpr std::array<std::pair<const char*, const struct Nif>, 3> NIFS = { {
    { "begin_/1", { { NIFFunctionType }, nif_begin } },
    { "get_board/0", { { NIFFunctionType }, nif_get_board } },
    { "update/0", { { NIFFunctionType }, nif_update } },
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

REGISTER_NIF_COLLECTION(m5, NULL, NULL, get_nif)

#endif
