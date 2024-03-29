/* SPDX-License-Identifier: MIT */
#include <sdkconfig.h>

#ifdef CONFIG_AVM_M5_I2C_ENABLE

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

#define MODULE_IN_PREFIX "m5_in_i2c:"
#define MODULE_EX_PREFIX "m5_ex_i2c:"

#define MAKE_ATOM(ctx, len, str) globalcontext_make_atom(ctx->global, ATOM_STR(len, str))

M5_NIF_ici2_v(nif_in_i2c_set_port, In_I2C, setPort, i2c_port_t)
M5_NIF_ici2_v(nif_ex_i2c_set_port, Ex_I2C, setPort, i2c_port_t)
M5_NIF_ici2_v(nif_in_i2c_begin, In_I2C, begin, i2c_port_t)
M5_NIF_ici2_v(nif_ex_i2c_begin, Ex_I2C, begin, i2c_port_t)

static constexpr std::array<std::pair<const char*, const struct Nif>, 2> IN_NIFS = { {
    { "set_port/3", { { NIFFunctionType }, nif_in_i2c_set_port } },
    { "begin_/3", { { NIFFunctionType }, nif_in_i2c_begin } },
} };

static constexpr std::array<std::pair<const char*, const struct Nif>, 2> EX_NIFS = { {
    { "set_port/3", { { NIFFunctionType }, nif_ex_i2c_set_port } },
    { "begin_/3", { { NIFFunctionType }, nif_ex_i2c_begin } },
} };

//
// Component Nif Entrypoints
//

static const struct Nif* get_nif(const char* nifname)
{
    if (memcmp(nifname, MODULE_IN_PREFIX, strlen(MODULE_IN_PREFIX)) == 0) {
        for (const auto& nif : IN_NIFS) {
            if (strcmp(nif.first, nifname + strlen(MODULE_IN_PREFIX)) == 0) {
                return &nif.second;
            }
        }
    } else if (memcmp(nifname, MODULE_EX_PREFIX, strlen(MODULE_EX_PREFIX)) == 0) {
        for (const auto& nif : EX_NIFS) {
            if (strcmp(nif.first, nifname + strlen(MODULE_EX_PREFIX)) == 0) {
                return &nif.second;
            }
        }
    }
    return NULL;
}

REGISTER_NIF_COLLECTION(m5_i2c, NULL, NULL, get_nif)

#endif
