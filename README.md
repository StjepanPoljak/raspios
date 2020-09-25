# RaspiOS

This is a little playground for AArch64 assembly on Raspberry Pi 3B+ (BCM2837B0 ARM Cortex A-53). It can be used and tweaked to bootstrap development on the chip. Current features are:

 * run from U-boot
 * run from any exception level to any lower exception level
 * UART functionality
 * print string and hex with customizable callbacks (here as UART)
 * exception / interrupt vector table
 * timer functionality

## Cross Compiler

RaspiOS uses the `aarch64-none-eabi` cross compiler available on ARM toolchain download site. To avoid installation anxieties, you can simply run `./install-cc.sh` script which will install the cross-compiler into the `cc` subfolder of your project. Thus, when running `make`, you can simply use:

```shell
PATH=cc/bin:$PATH make
```

## Build

You can use various options to build it, and the default values are already defined in Makefile under OPTIONS environment variable:

 * `BOOT_ADDR` - boot address (default is `0x01000000` which is basically `$kernel_addr_r` in U-Boot)
 * `BOOT_MODE` - specifies the target execution level (current execution level is determined at runtime)
 * `STACK_SIZE` - stack size (default is `0x1000`, does not have to be multiple of 16 bytes, as it will be aligned in the end (resulting in a somewhat larger stack)
 * `SECURE` - set as `1` to run target EL in secure or unsecure world
