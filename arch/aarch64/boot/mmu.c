#include <mmu.h>
#include <mem.h> /* raw_ptr */

#include <mair.h>
#include <tcr.h>
#include <pte.h>

#include "log.h"

#define MMU_TRACE
#define MMU_TESTS

#ifdef MMU_TRACE
#define _mmu_trace(suffix, value) print ## suffix(value)
#define mmu_trace(suffix, value, level) log(suffix, value, level)
#else
#define _mmu_trace(suffix, value) ;;
#define mmu_trace(suffix, value, level) ;;
#endif

#define pre_granule(s, t) TCR_TT ## t ## _GRANULE_ ## s
#define eval(s, t) pre_granule(s, t)
#define granule(t) eval(CONFIG_GRANULE_SIZE, t)

uint64_t l1_low[512] __ptable_boot = { 0 };
uint64_t l2_low[512] __ptable_boot = { 0 };

uint64_t l1_high[512] __ptable_boot = { 0 };
uint64_t l2_high[512] __ptable_boot = { 0 };
uint64_t l3_high[512] __ptable_boot = { 0 };

__boot addr_t addr_trans(addr_t va) {

	addr_t pa;

	__asm volatile(
		"at S1E1R, %1;"
		"mrs %0, par_el1;"
		: "=r" (pa)
		: "r" (va)
		);

	mmu_trace(, "virt_to_phys(", LOG_INFO);
	_mmu_trace(64, va);
	_mmu_trace(, ") = ");

	if (pa & 0x1) {
		_mmu_trace(ln, "NULL");
		return 0;
	}

	pa &= 0xfffffffff000;

	_mmu_trace(64, pa);
	_mmu_trace(ln, "");

	return pa;
}

#define load_table(table, table_base) do { \
	__asm volatile( \
		"msr ttbr" table "_el1, %0;" \
		:: "r" (table_base) \
	); \
	mmu_trace(, "Loaded ttbr" table "_el1@", LOG_INFO); \
	_mmu_trace(64, table_base); \
	_mmu_trace(ln, ""); \
} while(0) \

/* MAIR-related */

static __boot void mmu_set_mair(void) {

	reg_t mair = (MAIR_DEVICE << (MAIR_DEVICE_INDEX * 8)) |
		     (MAIR_CACHEABLE << (MAIR_CACHEABLE_INDEX * 8)) |
		     (MAIR_NON_CACHEABLE << (MAIR_NON_CACHEABLE_INDEX * 8));

	mmu_trace(, "Saving MAIR (", LOG_INFO);
	_mmu_trace(64, mair);
	_mmu_trace(ln, ").");
	
	__asm volatile(
		"msr mair_el1, %0;"
		: // no output
		: "r" (mair));

	return;
}

/* TCR-related */

static __boot void mmu_set_tcr(void) {

	reg_t tcr = 0;

	/* TTBR1 */
	tcr |= (64 - CONFIG_VA_BITS);
	tcr |= TCR_MISS_NO_FAULT;
	tcr |= (TCR_CACHEABLE_WB_WA << 8);
	tcr |= (TCR_CACHEABLE_WB_WA << 10);
	tcr |= TCR_INNER_SHAREABLE;
	tcr <<= 16; /* it's important to keep these
		     * after the shift! */
	tcr |= (TCR_TOP_BYTE_USED << 1);
	tcr |= granule(0);

	/* TTBR0 */
	tcr |= (64 - CONFIG_VA_BITS);
	tcr |= TCR_MISS_NO_FAULT;
	tcr |= (TCR_CACHEABLE_WB_WA << 8);
	tcr |= (TCR_CACHEABLE_WB_WA << 10);
	tcr |= TCR_INNER_SHAREABLE;
	tcr |= granule(0);
	tcr |= TCR_TOP_BYTE_USED;

	/* common */
	tcr |= TCR_IPA_32BIT;
	tcr |= TCR_ASID_TTBR0;
	tcr |= TCR_ASID_8BIT;

	mmu_trace(, "Setting TCR: ", LOG_INFO);
	_mmu_trace(64, tcr);
	_mmu_trace(ln, "");

	__asm volatile(
		"msr tcr_el1, %0;"
		"isb;"
		: // no output
		: "r" (tcr)
	);

	__asm volatile(
		"mrs %0, tcr_el1; isb"
		: "=r" (tcr) :
	);

	mmu_trace(, "Set TCR: ", LOG_INFO);
	_mmu_trace(64, tcr);
	_mmu_trace(ln, "");

}

extern addr_t UARTBASE;
extern addr_t _kernel_phys_start;

__boot void map_early(uint64_t *l1, uint64_t *l2) {
	uint16_t i;
	pentry_t curr_addr;

	/* map first GB for kernel */
	l1[0] = (pentry_t)PT_TABLE_DESC;
	l1[0] |= (pentry_t)raw_ptr(l2);

	for (i = 0; i < 512; i++) {
		curr_addr = (pentry_t)(L2_SIZE_PER_ENTRY * (uint64_t)i);
		/* do it now like this, deal with other devices later */
		if ((UARTBASE >= curr_addr) && (UARTBASE < curr_addr + L2_SIZE_PER_ENTRY)) {
			l2[i] = (pentry_t)(curr_addr | PE_DEVICE | PT_BLOCK_ENTRY);
		} else {
			l2[i] = (pentry_t)(curr_addr | PE_KERNEL_CODE | PT_BLOCK_ENTRY);
		}
	}

	return;
}


__boot void map_early_high(uint64_t *l1, uint64_t *l2, uint64_t *l3) {
	uint8_t wrote_4k;
	uint16_t i;
	pentry_t curr_addr;

	wrote_4k = 0;

	/* map first GB for kernel */
	l1[0] = (pentry_t)PT_TABLE_DESC;
	l1[0] |= (pentry_t)raw_ptr(l2);

	for (i = 0; i < 512; i++) {
		curr_addr = (pentry_t)(L3_SIZE_PER_ENTRY * (uint64_t)i + (uint64_t)(&_kernel_phys_start));
		if (curr_addr % L2_SIZE_PER_ENTRY == 0) {
			break;
		}
	        /* for L3 we use PT_TABLE_DESC instead of PT_BLOCK_ENTRY */
		l3[i] = (pentry_t)(curr_addr | PE_KERNEL_CODE | PT_TABLE_DESC);
		
		if (wrote_4k == 0) {
			wrote_4k = 1;
			l2[0] = (pentry_t)PT_TABLE_DESC;
			l2[0] |= (pentry_t)raw_ptr(l3);
		}
	}

	/* fix this to offset for last address ? */
	for (i = wrote_4k ? 1 : 0; i < 512; i++) {
		curr_addr = (pentry_t)(L2_SIZE_PER_ENTRY * (uint64_t)i + ((uint64_t)(&_kernel_phys_start) / 0x200000));
		/* do it now like this, deal with other devices later */
//		if ((UARTBASE >= curr_addr) && (UARTBASE < curr_addr + L2_SIZE_PER_ENTRY)) {
//			l2[i] = (pentry_t)(curr_addr | PE_DEVICE | PT_BLOCK_ENTRY);
//		} else {
			l2[i] = (pentry_t)(curr_addr | PE_KERNEL_CODE | PT_BLOCK_ENTRY);
//		}
	}

	return;
}

/*
__boot void jump(void) {
	asm volatile(
		"ldr x0, =high_va_entry;"
		"mov x1, 0x80000;"
		"add x0, x0, x1;"
		"br      x0");
}
*/
__boot void mmu_init(void) {

	load_table("0", raw_ptr(l1_low));
	load_table("1", raw_ptr(l1_high));

	map_early(l1_low, l2_low);
	map_early_high(l1_high, l2_high, l3_high);

	mmu_set_mair();
	mmu_set_tcr();

	asm volatile(
		"dsb ish; isb; msr sctlr_el1, %0;"
		"isb; nop; nop; nop; nop"
		:
		:"r"(0x5 | (1 << 12))
	);

	mmu_trace(ln, "MMU initialized.", LOG_INFO);

	return;
}

