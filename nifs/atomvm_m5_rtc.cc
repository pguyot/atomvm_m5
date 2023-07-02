/* SPDX-License-Identifier: MIT */
#include <sdkconfig.h>

#ifdef CONFIG_AVM_M5_RTC_ENABLE

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

#define MODULE_PREFIX "m5_rtc:"

#define MAKE_ATOM(ctx, len, str) globalcontext_make_atom(ctx->global, ATOM_STR(len, str))

static term nif_rtc_is_enabled(Context* ctx, int argc, term argv[])
{
    UNUSED(argc);
    UNUSED(argv);

    return M5.Rtc.isEnabled() ? TRUE_ATOM : FALSE_ATOM;
}

static term nif_rtc_get_time(Context* ctx, int argc, term argv[])
{
    UNUSED(argc);
    UNUSED(argv);

    m5::rtc_time_t time;
    M5.Rtc.getTime(&time);
    if (memory_ensure_free_opt(ctx, TUPLE_SIZE(3), MEMORY_CAN_SHRINK) != MEMORY_GC_OK) {
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    }
    term result = term_alloc_tuple(3, &ctx->heap);
    term_put_tuple_element(result, 0, term_from_int(time.hours));
    term_put_tuple_element(result, 1, term_from_int(time.minutes));
    term_put_tuple_element(result, 2, term_from_int(time.seconds));

    return result;
}

static term nif_rtc_get_date(Context* ctx, int argc, term argv[])
{
    UNUSED(argc);
    UNUSED(argv);

    m5::rtc_date_t date;
    M5.Rtc.getDate(&date);
    if (memory_ensure_free_opt(ctx, TUPLE_SIZE(3), MEMORY_CAN_SHRINK) != MEMORY_GC_OK) {
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    }
    term result = term_alloc_tuple(3, &ctx->heap);
    term_put_tuple_element(result, 0, term_from_int(date.year));
    term_put_tuple_element(result, 1, term_from_int(date.month));
    term_put_tuple_element(result, 2, term_from_int(date.date));

    return result;
}

static term nif_rtc_get_datetime(Context* ctx, int argc, term argv[])
{
    UNUSED(argc);
    UNUSED(argv);

    m5::rtc_datetime_t datetime;
    M5.Rtc.getDateTime(&datetime);
    if (memory_ensure_free_opt(ctx, TUPLE_SIZE(2) + 2 * TUPLE_SIZE(3), MEMORY_CAN_SHRINK) != MEMORY_GC_OK) {
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    }
    term result = term_alloc_tuple(2, &ctx->heap);
    term date_tuple = term_alloc_tuple(3, &ctx->heap);
    term_put_tuple_element(date_tuple, 0, term_from_int(datetime.date.year));
    term_put_tuple_element(date_tuple, 1, term_from_int(datetime.date.month));
    term_put_tuple_element(date_tuple, 2, term_from_int(datetime.date.date));
    term_put_tuple_element(result, 0, date_tuple);
    term time_tuple = term_alloc_tuple(3, &ctx->heap);
    term_put_tuple_element(time_tuple, 0, term_from_int(datetime.time.hours));
    term_put_tuple_element(time_tuple, 1, term_from_int(datetime.time.minutes));
    term_put_tuple_element(time_tuple, 2, term_from_int(datetime.time.seconds));
    term_put_tuple_element(result, 1, time_tuple);

    return result;
}

static term nif_rtc_set_time(Context* ctx, int argc, term argv[])
{
    UNUSED(argc);

    m5::rtc_time_t time;
    term time_tuple = argv[0];
    VALIDATE_VALUE(time_tuple, term_is_tuple);
    if (term_get_tuple_arity(time_tuple) != 3) {
        RAISE_ERROR(BADARG_ATOM);
    }
    time.hours = term_to_int(term_get_tuple_element(time_tuple, 0));
    time.minutes = term_to_int(term_get_tuple_element(time_tuple, 1));
    time.seconds = term_to_int(term_get_tuple_element(time_tuple, 2));
    M5.Rtc.setTime(time);

    return OK_ATOM;
}

static term nif_rtc_set_date(Context* ctx, int argc, term argv[])
{
    UNUSED(argc);

    m5::rtc_date_t date;
    term date_tuple = argv[0];
    VALIDATE_VALUE(date_tuple, term_is_tuple);
    if (term_get_tuple_arity(date_tuple) != 3) {
        RAISE_ERROR(BADARG_ATOM);
    }
    date.year = term_to_int(term_get_tuple_element(date_tuple, 0));
    date.month = term_to_int(term_get_tuple_element(date_tuple, 1));
    date.date = term_to_int(term_get_tuple_element(date_tuple, 2));
    date.weekDay = -1;
    M5.Rtc.setDate(date);

    return OK_ATOM;
}

static term nif_rtc_set_datetime(Context* ctx, int argc, term argv[])
{
    UNUSED(argc);

    m5::rtc_datetime_t datetime;
    term datetime_tuple = argv[0];
    VALIDATE_VALUE(datetime_tuple, term_is_tuple);
    if (term_get_tuple_arity(datetime_tuple) != 2) {
        RAISE_ERROR(BADARG_ATOM);
    }
    term date_tuple = term_get_tuple_element(datetime_tuple, 0);
    VALIDATE_VALUE(date_tuple, term_is_tuple);
    if (term_get_tuple_arity(date_tuple) != 3) {
        RAISE_ERROR(BADARG_ATOM);
    }
    term time_tuple = term_get_tuple_element(datetime_tuple, 1);
    VALIDATE_VALUE(time_tuple, term_is_tuple);
    if (term_get_tuple_arity(time_tuple) != 3) {
        RAISE_ERROR(BADARG_ATOM);
    }
    datetime.date.year = term_to_int(term_get_tuple_element(date_tuple, 0));
    datetime.date.month = term_to_int(term_get_tuple_element(date_tuple, 1));
    datetime.date.date = term_to_int(term_get_tuple_element(date_tuple, 2));
    datetime.date.weekDay = -1;
    datetime.time.hours = term_to_int(term_get_tuple_element(time_tuple, 0));
    datetime.time.minutes = term_to_int(term_get_tuple_element(time_tuple, 1));
    datetime.time.seconds = term_to_int(term_get_tuple_element(time_tuple, 2));
    M5.Rtc.setDateTime(datetime);

    return OK_ATOM;
}

static constexpr std::array<std::pair<const char*, const struct Nif>, 7> NIFS = { { { "is_enabled/0", { { NIFFunctionType }, nif_rtc_is_enabled } },
    { "get_time/0", { { NIFFunctionType }, nif_rtc_get_time } },
    { "get_date/0", { { NIFFunctionType }, nif_rtc_get_date } },
    { "get_datetime/0", { { NIFFunctionType }, nif_rtc_get_datetime } },
    { "set_time/1", { { NIFFunctionType }, nif_rtc_set_time } },
    { "set_date/1", { { NIFFunctionType }, nif_rtc_set_date } },
    { "set_datetime/1", { { NIFFunctionType }, nif_rtc_set_datetime } } } };

//
// Component Nif Entrypoints
//

static const struct Nif* get_nif(const char* nifname)
{
    if (memcmp(nifname, MODULE_PREFIX, strlen(MODULE_PREFIX)) == 0) {
        for (const auto& nif : NIFS) {
            if (strcmp(nif.first, nifname + strlen(MODULE_PREFIX)) == 0) {
                return &nif.second;
            }
        }
    }
    return NULL;
}

REGISTER_NIF_COLLECTION(m5_rtc, NULL, NULL, get_nif)

#endif
