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

Please refer to [examples](examples/) and [API documentation](https://pguyot.github.io/atomvm_m5/).
