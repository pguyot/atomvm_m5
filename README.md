# AtomVM support for M5

This project is a port of M5Unified for the AtomVM platform.

## Installation

- Install ESP-IDF SDK 5.1 to 5.5 (5.5 recommended)
- Checkout [atomvm](https://github.com/AtomVM/AtomVM)
- Within `src/platform/esp32/components`, checkout this project
- Activate ESP-IDF environment and within `src/platform/esp32/` run `idf.py build`

## Usage

Please refer to [examples](examples/) and [API documentation](https://pguyot.github.io/atomvm_m5/).

## M5Unified and M5GFX components

This port is based on the following versions of M5Unified/M5GFX:
- M5Unified [0.2.10](https://github.com/m5stack/M5Unified/releases/tag/0.2.10)
- M5GFX [0.2.16](https://github.com/m5stack/M5GFX/releases/tag/0.2.16) (0.2.17 is currently incompatible with AtomVM)
