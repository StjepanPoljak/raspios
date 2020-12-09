#ifndef MMU_H
#define MMU_H

#define entry_no_t uint16_t
#define entry_t uint64_t

#define sync_all() __asm volatile("dsb sy")
#define invalidate_tlbs_el(el) \
	__asm volatile( \
		"tlbi vmalle" #el ";" \
		"dsb sy;" \
		"isb")

/* MMU functions */

void mmu_init(void);

#endif
