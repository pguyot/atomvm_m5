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
//#define ENABLE_TRACE
#include <trace.h>

#define MODULE_IN_PREFIX "m5_in_i2c:"
#define MODULE_EX_PREFIX "m5_ex_i2c:"

#define MAKE_ATOM(ctx, len, str) globalcontext_make_atom(ctx->global, ATOM_STR(len, str))

static term nif_in_i2c_set_port(Context* ctx, int argc, term argv[])
{
    UNUSED(argc);
    M5.In_I2C.setPort(term_to_int32(argv[0]), term_to_int32(argv[1]), term_to_int32(argv[2]));
    return OK_ATOM;
}

static term nif_ex_i2c_set_port(Context* ctx, int argc, term argv[])
{
    UNUSED(argc);
    M5.Ex_I2C.setPort(term_to_int32(argv[0]), term_to_int32(argv[1]), term_to_int32(argv[2]));
    return OK_ATOM;
}

static term nif_in_i2c_begin(Context* ctx, int argc, term argv[])
{
    UNUSED(argc);
    bool result = M5.In_I2C.begin(term_to_int32(argv[0]), term_to_int32(argv[1]), term_to_int32(argv[2]));
    return result ? TRUE_ATOM : FALSE_ATOM;
}

static term nif_ex_i2c_begin(Context* ctx, int argc, term argv[])
{
    UNUSED(argc);
    bool result = M5.Ex_I2C.begin(term_to_int32(argv[0]), term_to_int32(argv[1]), term_to_int32(argv[2]));
    return result ? TRUE_ATOM : FALSE_ATOM;
}

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
