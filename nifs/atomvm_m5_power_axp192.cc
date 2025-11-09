/* SPDX-License-Identifier: MIT */
#include <sdkconfig.h>

#ifdef CONFIG_AVM_M5_POWER_AXP192_ENABLE

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

#define MODULE_PREFIX "m5_power_axp192:"

#define MAKE_ATOM(ctx, len, str) globalcontext_make_atom(ctx->global, ATOM_STR(len, str))

#define NIF_IMPL_GETTER_INT(name, expr)                                      \
    static term nif_power_axp192_##name(Context* ctx, int argc, term argv[]) \
    {                                                                        \
        UNUSED(argc);                                                        \
        UNUSED(argv);                                                        \
        return term_from_int(M5.Power.Axp192.expr);                          \
    }
#define NIF_IMPL_GETTER_FLOAT(name, expr)                                    \
    static term nif_power_axp192_##name(Context* ctx, int argc, term argv[]) \
    {                                                                        \
        UNUSED(argc);                                                        \
        UNUSED(argv);                                                        \
        return term_from_float(M5.Power.Axp192.expr, &ctx->heap);            \
    }

NIF_IMPL_GETTER_INT(get_battery_level, getBatteryLevel())
NIF_IMPL_GETTER_FLOAT(get_battery_voltage, getBatteryVoltage())
NIF_IMPL_GETTER_FLOAT(get_battery_discharge_current, getBatteryDischargeCurrent())
NIF_IMPL_GETTER_FLOAT(get_battery_charge_current, getBatteryChargeCurrent())
NIF_IMPL_GETTER_FLOAT(get_battery_power, getBatteryPower())
NIF_IMPL_GETTER_FLOAT(get_acin_voltage, getACINVoltage())
NIF_IMPL_GETTER_FLOAT(get_acin_current, getACINCurrent())
NIF_IMPL_GETTER_FLOAT(get_vbus_voltage, getVBUSVoltage())
NIF_IMPL_GETTER_FLOAT(get_vbus_current, getVBUSCurrent())
NIF_IMPL_GETTER_FLOAT(get_aps_voltage, getAPSVoltage())
NIF_IMPL_GETTER_FLOAT(get_internal_temperature, getInternalTemperature())

static constexpr std::array<std::pair<const char*, const struct Nif>, 11> NIFS = { {
    { "get_battery_level/0", { { NIFFunctionType }, nif_power_axp192_get_battery_level } },
    { "get_battery_voltage/0", { { NIFFunctionType }, nif_power_axp192_get_battery_voltage } },
    { "get_battery_discharge_current/0", { { NIFFunctionType }, nif_power_axp192_get_battery_discharge_current } },
    { "get_battery_charge_current/0", { { NIFFunctionType }, nif_power_axp192_get_battery_charge_current } },
    { "get_battery_power/0", { { NIFFunctionType }, nif_power_axp192_get_battery_power } },
    { "get_acin_voltage/0", { { NIFFunctionType }, nif_power_axp192_get_acin_voltage } },
    { "get_acin_current/0", { { NIFFunctionType }, nif_power_axp192_get_acin_current } },
    { "get_vbus_voltage/0", { { NIFFunctionType }, nif_power_axp192_get_vbus_voltage } },
    { "get_vbus_current/0", { { NIFFunctionType }, nif_power_axp192_get_vbus_current } },
    { "get_aps_voltage/0", { { NIFFunctionType }, nif_power_axp192_get_aps_voltage } },
    { "get_internal_temperature/0", { { NIFFunctionType }, nif_power_axp192_get_internal_temperature } },
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

REGISTER_NIF_COLLECTION(m5_power_axp192, NULL, NULL, get_nif)

#endif
