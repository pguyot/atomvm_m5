/* SPDX-License-Identifier: MIT */
#include <sdkconfig.h>

#ifdef CONFIG_AVM_M5_IMU_ENABLE

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

#define MODULE_PREFIX "m5_imu:"

#define MAKE_ATOM(ctx, len, str) globalcontext_make_atom(ctx->global, ATOM_STR(len, str))

M5_NIF_v_b(nif_imu_is_enabled, Imu, isEnabled)

static term nif_imu_get_type(Context* ctx, int argc, term argv[])
{
    UNUSED(argc);
    UNUSED(argv);

    switch (M5.Imu.getType()) {
    case m5::imu_t::imu_none:
        return MAKE_ATOM(ctx, "\x4", "none");
    case m5::imu_t::imu_unknown:
        return MAKE_ATOM(ctx, "\x7", "unknown");
    case m5::imu_t::imu_sh200q:
        return MAKE_ATOM(ctx, "\x6", "sh200q");
    case m5::imu_t::imu_mpu6050:
        return MAKE_ATOM(ctx, "\x7", "mpu6050");
    case m5::imu_t::imu_mpu6886:
        return MAKE_ATOM(ctx, "\x7", "mpu6886");
    case m5::imu_t::imu_mpu9250:
        return MAKE_ATOM(ctx, "\x7", "mpu9250");
    case m5::imu_t::imu_bmi270:
        return MAKE_ATOM(ctx, "\x6", "bmi270");
    }

    return UNDEFINED_ATOM;
}

M5_NIF_v_3b(nif_imu_get_accel, Imu, getAccel)
M5_NIF_v_3b(nif_imu_get_gyro, Imu, getGyro)
M5_NIF_v_3b(nif_imu_get_mag, Imu, getMag)

static constexpr std::array<std::pair<const char*, const struct Nif>, 5> NIFS = { { { "is_enabled/0", { { NIFFunctionType }, nif_imu_is_enabled } },
    { "get_type/0", { { NIFFunctionType }, nif_imu_get_type } },
    { "get_accel/0", { { NIFFunctionType }, nif_imu_get_accel } },
    { "get_gyro/0", { { NIFFunctionType }, nif_imu_get_gyro } },
    { "get_mag/0", { { NIFFunctionType }, nif_imu_get_mag } } } };

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

REGISTER_NIF_COLLECTION(m5_imu, NULL, NULL, get_nif)

#endif
