% SPDX - License - Identifier : MIT
-module(m5_power_axp192).

-export([
    get_battery_level/0,
    get_battery_voltage/0,
    get_battery_discharge_current/0,
    get_battery_charge_current/0,
    get_battery_power/0,
    get_acin_voltage/0,
    get_acin_current/0,
    get_vbus_voltage/0,
    get_vbus_current/0,
    get_aps_voltage/0,
    get_internal_temperature/0
]).

%% @doc Return battery level
%% @returns battery level from 0 to 100
-spec get_battery_level() -> integer().
get_battery_level() -> throw(nif_error).

%% @doc Return battery voltage
%% @returns battery voltage
-spec get_battery_voltage() -> float().
get_battery_voltage() -> throw(nif_error).

%% @doc Return battery discharge current
%% @returns battery discharge current
-spec get_battery_discharge_current() -> float().
get_battery_discharge_current() -> throw(nif_error).

%% @doc Return battery charge current
%% @returns battery charge current
-spec get_battery_charge_current() -> float().
get_battery_charge_current() -> throw(nif_error).

%% @doc Return battery power
%% @returns battery power
-spec get_battery_power() -> float().
get_battery_power() -> throw(nif_error).

%% @doc Return ACIN voltage
%% @returns ACIN voltage
-spec get_acin_voltage() -> float().
get_acin_voltage() -> throw(nif_error).

%% @doc Return ACIN current
%% @returns ACIN current
-spec get_acin_current() -> float().
get_acin_current() -> throw(nif_error).

%% @doc Return VBUS voltage
%% @returns VBUS voltage
-spec get_vbus_voltage() -> float().
get_vbus_voltage() -> throw(nif_error).

%% @doc Return VBUS current
%% @returns VBUS current
-spec get_vbus_current() -> float().
get_vbus_current() -> throw(nif_error).

%% @doc Return APS voltage
%% @returns APS voltage
-spec get_aps_voltage() -> float().
get_aps_voltage() -> throw(nif_error).

%% @doc Return internal temperature
%% @returns internal temperature
-spec get_internal_temperature() -> float().
get_internal_temperature() -> throw(nif_error).
