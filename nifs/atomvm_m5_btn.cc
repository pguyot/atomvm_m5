/* SPDX-License-Identifier: MIT */
#include <sdkconfig.h>

#ifdef CONFIG_AVM_M5_BTN_ENABLE

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

#include "atomvm_m5_nifs.h"

#define MODULE_BTN_PWR_PREFIX "m5_btn_pwr:"
#define MODULE_BTN_A_PREFIX "m5_btn_a:"
#define MODULE_BTN_B_PREFIX "m5_btn_b:"
#define MODULE_BTN_C_PREFIX "m5_btn_c:"
#define MODULE_BTN_EXT_PREFIX "m5_btn_ext:"

#define MAKE_ATOM(ctx, len, str) globalcontext_make_atom(ctx->global, ATOM_STR(len, str))

#define M5_NIF_BUTTON(btn, module, nifs)                                                                                                                            \
    M5_NIF_v_b(nif_btn_##btn##_was_clicked, module, wasClicked)                                                                                                     \
    M5_NIF_v_b(nif_btn_##btn##_was_hold, module, wasHold)                                                                                                           \
    M5_NIF_v_b(nif_btn_##btn##_was_single_clicked, module, wasSingleClicked)                                                                                        \
    M5_NIF_v_b(nif_btn_##btn##_was_double_clicked, module, wasDoubleClicked)                                                                                        \
    M5_NIF_v_b(nif_btn_##btn##_was_decide_click_count, module, wasDecideClickCount)                                                                                 \
    M5_NIF_v_i(nif_btn_##btn##_get_click_count, module, getClickCount)                                                                                              \
    M5_NIF_v_b(nif_btn_##btn##_is_holding, module, isHolding)                                                                                                       \
    M5_NIF_v_b(nif_btn_##btn##_was_change_pressed, module, wasChangePressed)                                                                                        \
    M5_NIF_v_b(nif_btn_##btn##_is_pressed, module, isPressed)                                                                                                       \
    M5_NIF_v_b(nif_btn_##btn##_is_released, module, isReleased)                                                                                                     \
    M5_NIF_v_b(nif_btn_##btn##_was_pressed, module, wasPressed)                                                                                                     \
    M5_NIF_v_b(nif_btn_##btn##_was_released, module, wasReleased)                                                                                                   \
    M5_NIF_v_b(nif_btn_##btn##_was_released_after_hold, module, wasReleasedAfterHold)                                                                               \
    M5_NIF_i_b(nif_btn_##btn##_was_released_for, module, wasReleaseFor)                                                                                             \
        M5_NIF_i_b(nif_btn_##btn##_pressed_for, module, pressedFor)                                                                                                 \
            M5_NIF_i_b(nif_btn_##btn##_release_for, module, releasedFor)                                                                                            \
                M5_NIF_i_v(nif_btn_##btn##_set_debounce_thresh, module, setDebounceThresh)                                                                          \
    M5_NIF_i_v(nif_btn_##btn##_set_hold_thresh, module, setHoldThresh)                                                                                              \
    M5_NIF_v_i(nif_btn_##btn##_last_change, module, lastChange)                                                                                                     \
    M5_NIF_v_i(nif_btn_##btn##_get_debounce_thresh, module, getDebounceThresh)                                                                                      \
    M5_NIF_v_i(nif_btn_##btn##_get_hold_thresh, module, getHoldThresh)                                                                                              \
    M5_NIF_v_i(nif_btn_##btn##_get_update_msec, module, getUpdateMsec)                                                                                              \
    static constexpr std::array<std::pair<const char*, const struct Nif>, 22> nifs = { { { "was_clicked/0", { { NIFFunctionType }, nif_btn_##btn##_was_clicked } }, \
        { "was_hold/0", { { NIFFunctionType }, nif_btn_##btn##_was_hold } },                                                                                        \
        { "was_single_clicked/0", { { NIFFunctionType }, nif_btn_##btn##_was_single_clicked } },                                                                    \
        { "was_double_clicked/0", { { NIFFunctionType }, nif_btn_##btn##_was_double_clicked } },                                                                    \
        { "was_decide_click_count/0", { { NIFFunctionType }, nif_btn_##btn##_was_decide_click_count } },                                                            \
        { "get_click_count/0", { { NIFFunctionType }, nif_btn_##btn##_get_click_count } },                                                                          \
        { "is_holding/0", { { NIFFunctionType }, nif_btn_##btn##_is_holding } },                                                                                    \
        { "was_change_pressed/0", { { NIFFunctionType }, nif_btn_##btn##_was_change_pressed } },                                                                    \
        { "is_pressed/0", { { NIFFunctionType }, nif_btn_##btn##_is_pressed } },                                                                                    \
        { "is_released/0", { { NIFFunctionType }, nif_btn_##btn##_is_released } },                                                                                  \
        { "was_pressed/0", { { NIFFunctionType }, nif_btn_##btn##_was_pressed } },                                                                                  \
        { "was_released/0", { { NIFFunctionType }, nif_btn_##btn##_was_released } },                                                                                \
        { "was_released_after_hold/0", { { NIFFunctionType }, nif_btn_##btn##_was_released_after_hold } },                                                          \
        { "was_released_for/1", { { NIFFunctionType }, nif_btn_##btn##_was_released_for } },                                                                        \
        { "pressed_for/1", { { NIFFunctionType }, nif_btn_##btn##_pressed_for } },                                                                                  \
        { "release_for/1", { { NIFFunctionType }, nif_btn_##btn##_release_for } },                                                                                  \
        { "set_debounce_thresh/1", { { NIFFunctionType }, nif_btn_##btn##_set_debounce_thresh } },                                                                  \
        { "set_hold_thresh/1", { { NIFFunctionType }, nif_btn_##btn##_set_hold_thresh } },                                                                          \
        { "last_change/0", { { NIFFunctionType }, nif_btn_##btn##_last_change } },                                                                                  \
        { "get_debounce_thresh/0", { { NIFFunctionType }, nif_btn_##btn##_get_debounce_thresh } },                                                                  \
        { "get_hold_thresh/0", { { NIFFunctionType }, nif_btn_##btn##_get_hold_thresh } },                                                                          \
        { "get_update_msec/0", { { NIFFunctionType }, nif_btn_##btn##_get_update_msec } } } }

M5_NIF_BUTTON(pwr, BtnPWR, PWR_NIFS);
M5_NIF_BUTTON(a, BtnA, A_NIFS);
M5_NIF_BUTTON(b, BtnB, B_NIFS);
M5_NIF_BUTTON(c, BtnC, C_NIFS);
M5_NIF_BUTTON(ext, BtnEXT, EXT_NIFS);

//
// Component Nif Entrypoints
//

static const struct Nif* get_nif(const char* nifname)
{
    if (memcmp(nifname, MODULE_BTN_PWR_PREFIX, strlen(MODULE_BTN_PWR_PREFIX)) == 0) {
        for (const auto& nif : PWR_NIFS) {
            if (strcmp(nif.first, nifname + strlen(MODULE_BTN_PWR_PREFIX)) == 0) {
                return &nif.second;
            }
        }
    } else if (memcmp(nifname, MODULE_BTN_A_PREFIX, strlen(MODULE_BTN_A_PREFIX)) == 0) {
        for (const auto& nif : A_NIFS) {
            if (strcmp(nif.first, nifname + strlen(MODULE_BTN_A_PREFIX)) == 0) {
                return &nif.second;
            }
        }
    } else if (memcmp(nifname, MODULE_BTN_B_PREFIX, strlen(MODULE_BTN_B_PREFIX)) == 0) {
        for (const auto& nif : B_NIFS) {
            if (strcmp(nif.first, nifname + strlen(MODULE_BTN_B_PREFIX)) == 0) {
                return &nif.second;
            }
        }
    } else if (memcmp(nifname, MODULE_BTN_C_PREFIX, strlen(MODULE_BTN_C_PREFIX)) == 0) {
        for (const auto& nif : C_NIFS) {
            if (strcmp(nif.first, nifname + strlen(MODULE_BTN_C_PREFIX)) == 0) {
                return &nif.second;
            }
        }
    } else if (memcmp(nifname, MODULE_BTN_EXT_PREFIX, strlen(MODULE_BTN_EXT_PREFIX)) == 0) {
        for (const auto& nif : EXT_NIFS) {
            if (strcmp(nif.first, nifname + strlen(MODULE_BTN_EXT_PREFIX)) == 0) {
                return &nif.second;
            }
        }
    }
    return NULL;
}

REGISTER_NIF_COLLECTION(m5_btn, NULL, NULL, get_nif)

#endif
