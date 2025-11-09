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
// #define ENABLE_TRACE
#include <trace.h>

#include "atomvm_m5_nifs.h"

#define MODULE_PREFIX "m5_speaker:"

M5_NIF_v_b(nif_speaker_is_enabled, Speaker, isEnabled)
M5_NIF_v_b(nif_speaker_is_playing, Speaker, isPlaying)
M5_NIF_i_v(nif_speaker_set_volume, Speaker, setVolume)

static term nif_speaker_tone(Context* ctx, int argc, term argv[])
{
    VALIDATE_VALUE(argv[0], term_is_number);
    VALIDATE_VALUE(argv[1], term_is_integer);
    int channel = -1;
    if (argc > 2) {
        VALIDATE_VALUE(argv[2], term_is_integer);
        channel = term_to_int(argv[2]);
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

template <typename T>
static term nif_speaker_play_raw(Context* ctx, int argc, term argv[])
{
    UNUSED(argc);
    VALIDATE_VALUE(argv[0], term_is_binary);
    uint32_t sample_rate = 44100;
    bool stereo = false;
    uint32_t repeat = 1;
    int channel = -1;
    bool stop_current_sound = false;

    if (argc > 1) {
        VALIDATE_VALUE(argv[1], term_is_integer);
        sample_rate = term_to_int(argv[1]);
        if (argc > 2) {
            VALIDATE_VALUE(argv[2], term_is_atom);
            stereo = argv[2] == TRUE_ATOM;
            if (argc > 3) {
                VALIDATE_VALUE(argv[3], term_is_integer);
                repeat = term_to_int(argv[3]);
                if (argc > 4) {
                    VALIDATE_VALUE(argv[4], term_is_integer);
                    channel = term_to_int(argv[4]);
                    if (argc > 5) {
                        VALIDATE_VALUE(argv[5], term_is_atom);
                        stop_current_sound = argv[5] == TRUE_ATOM;
                    }
                }
            }
        }
    }

    const T* data = (const T*)term_binary_data(argv[0]);
    size_t data_len = term_binary_size(argv[0]) / sizeof(T);

    bool result = M5.Speaker.playRaw(data, data_len, sample_rate, stereo, repeat, channel, stop_current_sound);
    return result ? TRUE_ATOM : FALSE_ATOM;
}

static constexpr std::array<std::pair<const char*, const struct Nif>, 24> NIFS = { { { "is_enabled/0", { { NIFFunctionType }, nif_speaker_is_enabled } },
    { "is_playing/0", { { NIFFunctionType }, nif_speaker_is_playing } },
    { "set_volume/1", { { NIFFunctionType }, nif_speaker_set_volume } },
    { "tone/2", { { NIFFunctionType }, nif_speaker_tone } },
    { "tone/3", { { NIFFunctionType }, nif_speaker_tone } },
    { "tone/4", { { NIFFunctionType }, nif_speaker_tone } },
    { "play_raw_u8/1", { { NIFFunctionType }, nif_speaker_play_raw<uint8_t> } },
    { "play_raw_u8/2", { { NIFFunctionType }, nif_speaker_play_raw<uint8_t> } },
    { "play_raw_u8/3", { { NIFFunctionType }, nif_speaker_play_raw<uint8_t> } },
    { "play_raw_u8/4", { { NIFFunctionType }, nif_speaker_play_raw<uint8_t> } },
    { "play_raw_u8/5", { { NIFFunctionType }, nif_speaker_play_raw<uint8_t> } },
    { "play_raw_u8/6", { { NIFFunctionType }, nif_speaker_play_raw<uint8_t> } },
    { "play_raw_s8/1", { { NIFFunctionType }, nif_speaker_play_raw<int8_t> } },
    { "play_raw_s8/2", { { NIFFunctionType }, nif_speaker_play_raw<int8_t> } },
    { "play_raw_s8/3", { { NIFFunctionType }, nif_speaker_play_raw<int8_t> } },
    { "play_raw_s8/4", { { NIFFunctionType }, nif_speaker_play_raw<int8_t> } },
    { "play_raw_s8/5", { { NIFFunctionType }, nif_speaker_play_raw<int8_t> } },
    { "play_raw_s8/6", { { NIFFunctionType }, nif_speaker_play_raw<int8_t> } },
    { "play_raw_s16/1", { { NIFFunctionType }, nif_speaker_play_raw<int16_t> } },
    { "play_raw_s16/2", { { NIFFunctionType }, nif_speaker_play_raw<int16_t> } },
    { "play_raw_s16/3", { { NIFFunctionType }, nif_speaker_play_raw<int16_t> } },
    { "play_raw_s16/4", { { NIFFunctionType }, nif_speaker_play_raw<int16_t> } },
    { "play_raw_s16/5", { { NIFFunctionType }, nif_speaker_play_raw<int16_t> } },
    { "play_raw_s16/6", { { NIFFunctionType }, nif_speaker_play_raw<int16_t> } } } };

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
