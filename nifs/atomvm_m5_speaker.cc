/* SPDX-License-Identifier: MIT */
#include <sdkconfig.h>

#ifdef CONFIG_AVM_M5_SPEAKER_ENABLE

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
//#define ENABLE_TRACE
#include <trace.h>

#define MODULE_PREFIX "m5_speaker:"

static term nif_speaker_tone(Context* ctx, int argc, term argv[])
{
    VALIDATE_VALUE(argv[0], term_is_number);
    VALIDATE_VALUE(argv[1], term_is_integer);
    int channel = -1;
    if (argc > 2) {
        VALIDATE_VALUE(argv[2], term_is_number);
        channel = term_to_int32(argv[2]);
    }
    bool stop_current_sound = true;
    if (argc > 3) {
        if (argv[3] != TRUE_ATOM && argv[3] != FALSE_ATOM) {
            argv[0] = ERROR_ATOM;
            argv[1] = BADARG_ATOM;
            return term_invalid_term();
        }
        stop_current_sound = argv[3] == TRUE_ATOM;
    }
    avm_float_t frequency = term_conv_to_float(argv[0]);
    uint32_t duration = term_to_int32(argv[1]);
    bool r = M5.Speaker.tone(frequency, duration, channel, stop_current_sound);
    return r ? TRUE_ATOM : FALSE_ATOM;
}

static constexpr std::array<std::pair<const char*, const struct Nif>, 3> NIFS = { {
    { "tone/2", { { NIFFunctionType }, nif_speaker_tone } },
    { "tone/3", { { NIFFunctionType }, nif_speaker_tone } },
    { "tone/4", { { NIFFunctionType }, nif_speaker_tone } },

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

REGISTER_NIF_COLLECTION(m5_speaker, NULL, NULL, get_nif)

#endif
