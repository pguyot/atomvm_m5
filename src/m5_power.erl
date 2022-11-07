% SPDX-License-Identifier: MIT
-module(m5_power).

-export([
    set_ext_power/2,
    set_led/1,
    power_off/0,
    timer_sleep/1,
    deep_sleep/2,
    light_sleep/2,
    get_battery_level/0,
    set_battery_charge/1,
    set_charge_current/1,
    set_charge_voltage/1,
    is_charging/0
]).

-spec set_ext_power(Enable :: boolean(), PortMask :: 0..255) -> ok.
set_ext_power(_Enable, _PortMask) ->
    throw(nif_error).

-spec set_led(Brightness :: 0..255) -> ok.
set_led(_Brightness) ->
    throw(nif_error).

-spec power_off() -> ok.
power_off() ->
    throw(nif_error).

-spec timer_sleep(Seconds :: integer()) -> ok.
timer_sleep(_Seconds) ->
    throw(nif_error).

-spec deep_sleep(Microseconds :: integer(), TouchWakeup :: boolean()) -> no_return().
deep_sleep(_Microseconds, _TouchWakeup) ->
    throw(nif_error).

-spec light_sleep(Microseconds :: integer(), TouchWakeup :: boolean()) -> ok.
light_sleep(_Microseconds, _TouchWakeup) ->
    throw(nif_error).

-spec get_battery_level() -> 0..100.
get_battery_level() ->
    throw(nif_error).

-spec set_battery_charge(Enable :: boolean()) -> ok.
set_battery_charge(_Enable) ->
    throw(nif_error).

-spec set_charge_current(MaxMA :: integer()) -> ok.
set_charge_current(_MaxMA) ->
    throw(nif_error).

-spec set_charge_voltage(MaxMV :: integer()) -> ok.
set_charge_voltage(_MaxMV) ->
    throw(nif_error).

-spec is_charging() -> is_discharging | is_charging | charge_unknown.
is_charging() ->
    throw(nif_error).
