#ifndef COMMON_H
#define COMMON_H

#include <stdio.h>
#include <stdint.h>

#define logs_err(fmt, ...) printf("(!) " fmt "\n", ## __VA_ARGS__)
#define logs_inf(fmt, ...) printf("(i) " fmt "\n", ## __VA_ARGS__)

uint8_t* load_file(const char* infile, unsigned int* len);
const char* basename(const char* path);

#endif


