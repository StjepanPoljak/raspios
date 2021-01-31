#ifndef COMMON_H
#define COMMON_H

#include <stdio.h>

#define logs_err(fmt, ...) printf("(!) " fmt "\n", ## __VA_ARGS__)
#define logs_inf(fmt, ...) printf("(i) " fmt "\n", ## __VA_ARGS__)

#endif


