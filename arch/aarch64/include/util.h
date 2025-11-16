#ifndef __AARCH64_UTIL_H
#define __AARCH64_UTIL_H

#include <stdint.h>


static inline uint16_t bswap16(uint16_t b) {
	uint16_t ret = b;
	__asm volatile(
		"rev16 %w0, %w0;"
		: "+r" (ret)
		);
	return ret;
}

static inline uint32_t bswap32(uint32_t b) {
	uint32_t ret = b;
	__asm volatile(
		"rev %w0, %w0;"
		: "+r" (ret)
		);
	return ret;
}

static inline uint64_t bswap64(uint64_t b) {
	uint64_t ret = b;
	__asm volatile(
		"rev %0, %0;"
		: "+r" (ret)
		);
	return ret;
}

#endif
