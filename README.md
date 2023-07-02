# AtomVM support for M5

This project is a port of M5Unified for the AtomVM platform.

## Installation

- Install ESP-IDF SDK 4.4
- Checkout [atomvm](https://github.com/AtomVM/AtomVM)
- Within `src/platform/esp32/components`, checkout [M5Unified](https://github.com/m5stack/M5Unified)
- Within `src/platform/esp32/components`, checkout [M5GFX](https://github.com/m5stack/M5GFX)
- Within `src/platform/esp32/components`, checkout this project
- Activate ESP-IDF environment and within `src/platform/esp32/` run `idf.py build`

## Usage

The following modules are currently defined:

```
m5
m5_display
m5_i2c
m5_power
m5_power_axp192
m5_rtc
m5_speaker
```
