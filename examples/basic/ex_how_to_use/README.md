# How to use

This is the Elixir version of the erlang how_to_use example

This sample code is a port of the following example:
https://github.com/m5stack/M5Unified/blob/master/examples/Basic/HowToUse/HowToUse.ino

## Installation

- Compile and install AtomVM with `atomvm_m5` as explained [here](../../../README.md)
- run mix deps.get
- manually add those functions to deps/exatomvm/priv/funcs.txt in your project directory:
  - m5_imu:get_accel/0
  - m5_imu:get_gyro/0
  - m5_imu:get_type/0
  - m5_imu:is_enabled/0
  - m5_speaker:is_enabled/0
  - m5_speaker:is_playing/0
  - m5_speaker:play_raw_u8/3
  - m5_speaker:set_volume/1
  - m5_btn_a:was_pressed/0
  - m5_btn_a:was_released/0
- Connect a M5 device with AtomVM (VM and library) preinstalled.
- Compile with:

```
mix atomvm.packbeam
```

- and flash with:

```
sudo mix atomvm.esp32.flash --port /dev/tty.usbserial-*
```
