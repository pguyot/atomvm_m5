/* SPDX-License-Identifier: MIT */
#include <sdkconfig.h>

#ifdef CONFIG_AVM_M5_POWER_ENABLE

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

#define MODULE_PREFIX "m5_power:"

#define MAKE_ATOM(ctx, len, str) globalcontext_make_atom(ctx->global, ATOM_STR(len, str))

static term nif_power_deep_sleep(Context* ctx, int argc, term argv[])
{
    avm_int64_t usec = 0;
    bool touch_wakeup = true;
    if (argc > 0) {
        VALIDATE_VALUE(argv[0], term_is_any_integer);
        usec = term_maybe_unbox_int64(argv[0]);
        if (argc > 1) {
            VALIDATE_VALUE(argv[1], term_is_atom);
            touch_wakeup = argv[1] != FALSE_ATOM;
        }
    }
    M5.Power.deepSleep(usec, touch_wakeup);
    return OK_ATOM;
}

static term nif_power_timer_sleep(Context* ctx, int argc, term argv[])
{
    UNUSED(argc);
    VALIDATE_VALUE(argv[0], term_is_any_integer);
    M5.Power.timerSleep(term_to_int32(argv[0]));
    return OK_ATOM;
}

static term nif_power_get_battery_level(Context* ctx, int argc, term argv[])
{
    UNUSED(argc);
    UNUSED(argv);
    return term_from_int(M5.Power.getBatteryLevel());
}

static term nif_power_set_battery_charge(Context* ctx, int argc, term argv[])
{
    UNUSED(argc);
    VALIDATE_VALUE(argv[0], term_is_atom);
    M5.Power.setBatteryCharge(argv[0] == TRUE_ATOM);
    return OK_ATOM;
}

static term nif_power_set_charge_current(Context* ctx, int argc, term argv[])
{
    UNUSED(argc);
    VALIDATE_VALUE(argv[0], term_is_number);
    M5.Power.setChargeCurrent(term_conv_to_float(argv[0]));
    return OK_ATOM;
}

static term nif_power_set_charge_voltage(Context* ctx, int argc, term argv[])
{
    UNUSED(argc);
    VALIDATE_VALUE(argv[0], term_is_number);
    M5.Power.setChargeVoltage(term_conv_to_float(argv[0]));
    return OK_ATOM;
}

static term nif_power_is_charging(Context* ctx, int argc, term argv[])
{
    UNUSED(argc);
    UNUSED(argv);

    term result = term_nil();

    switch (M5.Power.isCharging()) {
    case m5::Power_Class::is_discharging:
        result = FALSE_ATOM;
        break;
    case m5::Power_Class::is_charging:
        result = TRUE_ATOM;
        break;
    case m5::Power_Class::charge_unknown:
        result = UNDEFINED_ATOM;
        break;
    }

    return result;
}

static term nif_power_get_type(Context* ctx, int argc, term argv[])
{
    UNUSED(argc);
    UNUSED(argv);

    term result = term_nil();

    switch (M5.Power.getType()) {
    case m5::Power_Class::pmic_unknown:
        result = MAKE_ATOM(ctx, "\x7", "unknown");
        break;
    case m5::Power_Class::pmic_adc:
        result = MAKE_ATOM(ctx, "\x3", "adc");
        break;
    case m5::Power_Class::pmic_axp192:
        result = MAKE_ATOM(ctx, "\x6", "axp192");
        break;
    case m5::Power_Class::pmic_axp2101:
        result = MAKE_ATOM(ctx, "\x7", "axp2101");
        break;
    case m5::Power_Class::pmic_ip5306:
        result = MAKE_ATOM(ctx, "\x6", "ip5306");
        break;
    }

    return result;
}

static constexpr std::array<std::pair<const char*, const struct Nif>, 10> NIFS = { {
    { "deep_sleep/0", { { NIFFunctionType }, nif_power_deep_sleep } },
    { "deep_sleep/1", { { NIFFunctionType }, nif_power_deep_sleep } },
    { "deep_sleep/2", { { NIFFunctionType }, nif_power_deep_sleep } },
    { "timer_sleep/1", { { NIFFunctionType }, nif_power_timer_sleep } },
    { "get_battery_level/0", { { NIFFunctionType }, nif_power_get_battery_level } },
    { "set_battery_charge/1", { { NIFFunctionType }, nif_power_set_battery_charge } },
    { "set_charge_current/1", { { NIFFunctionType }, nif_power_set_charge_current } },
    { "set_charge_voltage/1", { { NIFFunctionType }, nif_power_set_charge_voltage } },
    { "is_charging/0", { { NIFFunctionType }, nif_power_is_charging } },
    { "get_type/0", { { NIFFunctionType }, nif_power_get_type } },
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

REGISTER_NIF_COLLECTION(m5_power, NULL, NULL, get_nif)

#endif
