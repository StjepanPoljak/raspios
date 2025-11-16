#ifndef BRIDGE_H
#define BRIDGE_H

#include <attrs.h>
#include <stdint.h>

#define reg_t uint64_t


#define print(str) do { \
    static const char _boot_str[] __attribute__((section(".rodata.boot"))) = str; \
    print_string(_boot_str, uart_write_char);						\
} while (0)
#define println(string) print(string "\n\r")
#define print32(val) print_hex(val, uart_write_char, 4)
#define print32_raw(val) print_hex_raw(val, uart_write_char, 4)
#define print64(val) print_hex(val, uart_write_char, 8)
#define print64_raw(val) print_hex_raw(val, uart_write_char, 8)

#define TRUE (uint8_t)1
#define FALSE (uint8_t)0

__boot void uart_write_char(char);
__boot void print_string(const char*, void(*f)(char));
__boot void print_hex(reg_t, void(*f)(char), reg_t);
__boot void print_hex_raw(reg_t, void(*f)(char), reg_t);
__boot void newline();

#endif
