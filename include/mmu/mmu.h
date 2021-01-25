#ifndef MMU_H
#define MMU_H

#include <mem.h>

#define entry_no_t uint16_t
#define entry_t uint64_t

#define sync_all() __asm volatile("dsb sy")
#define invalidate_tlbs_el(el) \
	__asm volatile( \
		"tlbi vmalle" #el ";" \
		"dsb sy;" \
		"isb")

/* MMU functions */

addr_t addr_trans(addr_t va);
void mmu_init(void);

#endif
