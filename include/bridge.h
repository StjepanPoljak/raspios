#ifndef BRIDGE_H
#define BRIDGE_H

#include <stdint.h>

#define reg_t uint64_t

#define print(string) print_string(string, uart_write_char)
#define println(string) print(string "\n\r")
#define print64(val) print_hex(val, uart_write_char, 8)

#define TRUE (uint8_t)1
#define FALSE (uint8_t)0

void uart_write_char(char);
void print_string(const char*, void(*f)(char));
void print_hex(reg_t, void(*f)(char), reg_t);
void newline();

#endif
