% SPDX-License-Identifier: MIT
-module(m5_power).

-export([
    deep_sleep/0,
    deep_sleep/1,
    deep_sleep/2,
    timer_sleep/1,
    get_battery_level/0,
    set_battery_charge/1,
    set_charge_current/1,
    set_charge_voltage/1,
    is_charging/0,
    get_type/0
]).

-type charging_state() :: is_discharging | is_charging | charge_unknown.
-type power_type() :: adc | axp192 | axp2101 | ip5306 | unknown.

%% @doc Enter deep sleep
-spec deep_sleep() -> no_return().
deep_sleep() ->
    throw(nif_error).

%% @doc Enter deep sleep
%% @param Microseconds sleep for a given number of microseconds
-spec deep_sleep(Microseconds :: integer()) -> no_return().
deep_sleep(_Microseconds) ->
    throw(nif_error).

%% @doc Enter deep sleep
%% @param _Microseconds sleep for a given number of microseconds
%% @param _TouchWakeup if true, wake up on touch
-spec deep_sleep(Microseconds :: integer(), TouchWakeup :: boolean()) -> no_return().
deep_sleep(_Microseconds, _TouchWakeup) ->
    throw(nif_error).

%% @doc Sleep using timer
%% @param _Seconds sleep for a given number of seconds
-spec timer_sleep(Seconds :: integer()) -> ok.
timer_sleep(_Seconds) ->
    throw(nif_error).

%% @doc Get battery level
%% @returns Battery level from 0 to 100
-spec get_battery_level() -> 0..100.
get_battery_level() ->
    throw(nif_error).

%% @doc Enable or disable battery charge
%% @param _Enable if charge should be enabled
-spec set_battery_charge(Enable :: boolean()) -> ok.
set_battery_charge(_Enable) ->
    throw(nif_error).

%% @doc Set the charge current
%% @param _MaxMA Maximum charge current in mA
-spec set_charge_current(MaxMA :: number()) -> ok.
set_charge_current(_MaxMA) ->
    throw(nif_error).

%% @doc Set the charge voltage
%% @param _MaxMV Maximum charge voltage in mV
-spec set_charge_voltage(MaxMV :: integer()) -> ok.
set_charge_voltage(_MaxMV) ->
    throw(nif_error).

%% @doc Determine if battery is charging
%% @returns charging type depending on state and capability
-spec is_charging() -> charging_state().
is_charging() ->
    throw(nif_error).

%% @doc Get the power controller type
%% @returns the class of the power controller driver
-spec get_type() -> power_type().
get_type() ->
    throw(nif_error).
