% SPDX-License-Identifier: MIT
%% @doc Module to interact with on-board RTC chip.
%%
%% The date and time on the RTC can be different from the ESP32 datetime. On
%% start up (`m5:begin_()'), the library sets the system datetime from the time
%% in the RTC chip.
%%
%% Typical usage with the networking stack consists in updating the RTC datetime
%% when the time is obtained from network.
%%
-module(m5_rtc).

-export([
    is_enabled/0,
    get_time/0,
    get_date/0,
    get_datetime/0,
    set_time/1,
    set_date/1,
    set_datetime/1
]).

%% @doc Determine if RTC is enabled
%% @returns `true' if RTC is enabled.
-spec is_enabled() -> boolean().
is_enabled() ->
    throw(nif_error).

%% @doc Return time from the RTC
%% @returns time
-spec get_time() -> calendar:time().
get_time() ->
    throw(nif_error).

%% @doc Return date from the RTC
%% @returns date
-spec get_date() -> calendar:date().
get_date() ->
    throw(nif_error).

%% @doc Return date and time from the RTC
%% @returns date and time
-spec get_datetime() -> calendar:datetime().
get_datetime() ->
    throw(nif_error).

%% @doc Set the time on the RTC
%% @param _Time time to set
-spec set_time(calendar:time()) -> ok.
set_time(_Time) ->
    throw(nif_error).

%% @doc Set the date on the RTC
%% @param _Date date to set
-spec set_date(calendar:date()) -> ok.
set_date(_Date) ->
    throw(nif_error).

%% @doc Set date and time on the RTC
%% @param _Datetime date and time to set
-spec set_datetime(calendar:datetime()) -> ok.
set_datetime(_Datetime) ->
    throw(nif_error).
