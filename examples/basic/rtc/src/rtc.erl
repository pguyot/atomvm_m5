%% @doc RTC sample code
-module(rtc).

-export([start/0]).
-include("config.hrl").

start() ->
    m5:begin_([{clear_display, true}, {output_power, false}, {internal_mic, false}]),
    case m5_rtc:is_enabled() of
        true ->
            println("RTC found"),
            display_rtc_time(),
            println("Starting network"),
            ok = start_network(),
            wait_for_wifi();
        false ->
            io:format("RTC not found.\n"),
            m5_display:print(<<"RTC not found.">>)
    end.

start_network() ->
    Parent = self(),
    WifiConfig = [
        {sta, [
            {connected, fun() -> Parent ! {wifi, connected} end},
            {got_ip, fun(_Addr) -> Parent ! {wifi, got_ip} end},
            {ssid, ?WIFI_SSID},
            {psk, ?WIFI_PSK}
        ]},
        {sntp, [
            {host, "pool.ntp.org"}, {synchronized, fun(Timeval) -> Parent ! {ntp, Timeval} end}
        ]}
    ],
    case network:start(WifiConfig) of
        {ok, _Pid} ->
            println("Network started"),
            ok;
        Error ->
            println(io_lib:format("Network start failed:\n~p", [Error])),
            Error
    end.

wait_for_wifi() ->
    println("Wait for wifi"),
    receive
        {wifi, connected} ->
            wait_for_ip();
        Other ->
            println(io_lib:format("Unexpected message:\n~p", [Other])),
            wait_for_wifi()
    after 10000 ->
        io:format("Still waiting for wifi\n"),
        wait_for_wifi()
    end.

wait_for_ip() ->
    println("Wait for IP"),
    receive
        {wifi, got_ip} ->
            wait_for_ntp();
        Other ->
            println(io_lib:format("Unexpected message:\n~p", [Other])),
            wait_for_ip()
    after 10000 ->
        io:format("Still waiting for IP\n"),
        wait_for_ntp()
    end.

wait_for_ntp() ->
    println("Wait for NTP"),
    receive
        {ntp, Timeval} ->
            println(io_lib:format("Synchronized:\n~p", [Timeval])),
            set_rtc();
        Other ->
            println(io_lib:format("Unexpected message:\n~p", [Other])),
            wait_for_ntp()
    after 10000 ->
        io:format("Still waiting for NTP\n"),
        wait_for_ntp()
    end.

set_rtc() ->
    io:format("Setting M5 RTC to : ~p\n", [erlang:universaltime()]),
    m5_rtc:set_datetime(erlang:universaltime()),
    display_rtc_time().

display_rtc_time() ->
    io:format("M5 RTC time is now : ~p\n", [m5_rtc:get_datetime()]),
    DateTimeStr = iolist_to_binary(io_lib:format("RTC time:\n~p", [m5_rtc:get_datetime()])),
    m5_display:println(DateTimeStr).

println(IOData) ->
    io:format("~s\n", [IOData]),
    m5_display:println(IOData).
