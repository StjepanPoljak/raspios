# RaspiOS

This is a little playground for AArch64 assembly on Raspberry Pi 3B+ (BCM2837B0 ARM Cortex A-53). It can be used and tweaked to bootstrap development on the chip. Current features are:

 * run from U-boot
 * run from EL3 or EL2
 * UART functionality
 * print string and hex with customizable callbacks (here as UART)
 * exception / interrupt vector table
 * timer functionality
 * memory heap allocation
 * basic MMU mapping
 * basic GPU interface

## Cross Compiler

RaspiOS uses the `aarch64-none-elf` cross compiler available on ARM toolchain download site. This should be installed automatically by the build system. However, you can always try and avoid installation anxieties by running `./install-cc.sh` before starting the build.

## Configuration

You can use various options to build the OS, and the default values are already defined in `config.txt` file. Options for early start:

```
CONFIG_STACK_SIZE=0x1000
CONFIG_VA_BITS=39
CONFIG_SECURE=0
CONFIG_GRANULE_SIZE=4KB
```

The `CONFIG_SECURE` is about to be obsolete, and `CONFIG_VA_BITS` and `CONFIG_GRANULE_SIZE` aren't yet properly adapted to other values. There are also two more options for memory allocator debug:

 * `CONFIG_MEM_TESTS` - run some basic unit tests on memory initialization
 * `CONFIG_MEM_TRACE` - print detailed debug information on memory operations
 * `CONFIG_MBOX_TRACE` - trace mailbox functionality

Note: You can disable these last options simply by commenting them out with `#` sign.

## Build

To build RaspiOS, simply run:

```shell
mkdir build
cd build
ARCH=aarch64 cmake ..
make
```

## Known issues

 * GPU drivers are not working in QEMU (could be just temperature sensor, though)
 * timer triggers IRQ handler twice
