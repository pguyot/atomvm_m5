M5 RTC sample code
==================

![Sample code running on M5 Stick C Plus](./rtc.jpg)

This sample code illustrates how to use RTC and perform synchronization with NTP.

Installation
------------

- Compile and install AtomVM with `atomvm_m5` as explained [here](../../../README.md)
- Install rebar3
- Copy `src/config.hrl-template` to `src/config.hrl`
- Edit `src/config.hrl` with your Wifi credentials
- Connect a M5 device with AtomVM (VM and library) preinstalled.
- Compile and flash with:

```
rebar3 esp32_flash -p /dev/tty.usbserial-*
```
