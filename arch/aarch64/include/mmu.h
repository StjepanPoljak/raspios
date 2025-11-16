#ifndef AARCH64_MMU_H
#define AARCH64_MMU_H

#include <bridge.h>
#include <stdint.h>

#define addr_t uint64_t

#define L3_SIZE_PER_ENTRY 0x1000 /* 4KB */
#define L2_SIZE_PER_ENTRY 0x200000 /* 2MB */
#define L1_SIZE_PER_ENTRY 0x40000000 /* 1GB */
#define L0_SIZE_PER_ENTRY 0x8000000000 /* 512GB */

#define L0_ALIGNED(addr) (addr & ~(L0_SIZE_PER_ENTRY - 1))
#define L1_ALIGNED(addr) (addr & ~(L1_SIZE_PER_ENTRY - 1))
#define L2_ALIGNED(addr) (addr & ~(L2_SIZE_PER_ENTRY - 1))
#define L3_ALIGNED(addr) (addr & ~(L3_SIZE_PER_ENTRY - 1))

#define entry_no_t uint16_t
#define entry_t uint64_t

#define sync_all() __asm volatile("dsb sy")
#define invalidate_tlbs_el(el) \
	__asm volatile( \
		"tlbi vmalle" #el ";" \
		"dsb sy;" \
		"isb")

/* MMU functions */

__boot addr_t addr_trans(addr_t va);
__boot void mmu_init(void);

#endif
