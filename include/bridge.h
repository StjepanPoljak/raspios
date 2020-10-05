#ifndef BRIDGE_H
#define BRIDGE_H

#define print(string) print_string(string, uart_write_char)
#define println(string) print(string "\n\r")

void uart_write_char(char);
void print_string(const char*, void(*f)(char));

#endif
