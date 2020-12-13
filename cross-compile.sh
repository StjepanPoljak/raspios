#!/bin/sh

PATH=cc/bin:$PATH C_INCLUDE_PATH=cc/aarch64-none-elf/include make $@
