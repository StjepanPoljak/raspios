#include "mmu.h"

#include <stdint.h>

#include "bridge.h"

static uint8_t mmu_add_device_memory(uint8_t mair[8], uint8_t index,
				     enum mmu_device_attr dattr) {

	if (index >= 8 || index < 0)
		return 1;

	mair[index] = (uint8_t)dattr;

	return 0;
}

static uint8_t mmu_create_cacheable(
		enum mmu_shd shd,
		enum mmu_trans_attr trans,
		enum mmu_write_policy wp,
		enum mmu_read_alloc ra,
		enum mmu_write_alloc wa) {

	return (trans | wp | ra | wa) << (shd == MMU_OSH ? 4 : 0);
}

static uint8_t mmu_create_non_cacheable(enum mmu_shd shd) {

	return 0x4 << (shd == MMU_OSH ? 4 : 0);
}

static uint8_t mmu_add_normal_memory(uint8_t mair[8], uint8_t index,
				     uint8_t inner, uint8_t outer) {

	if (index >= 8 || index < 0)
		return 1;

	mair[index] = inner | outer;

	return 0;
}

static void mmu_save_mair(uint8_t mair[8]) {

	__asm volatile(
		"ldr x1, [%0];"
		"msr mair_el1, x1;"
		: // no output
		: "r" (mair));

	return;

}
