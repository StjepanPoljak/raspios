#include <mmu/mmu.h>

#include <mmu/mair.h>
#include <mmu/tcr.h>
#include <mmu/pte.h>

#include <mem.h>
#include <ptable.h>

#include <stdint.h>

#include "bridge.h"
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

ptable_t kpt;
ptable_t dpt;

static addr_t virt_to_phys(addr_t va) {

	addr_t pa;

	__asm volatile(
		"at S1E1R, %0;"
		"mrs %1, par_el1;"
		: "=r" (pa)
		: "r" (va)
		);

	mmu_trace(, "virt_to_phys(", LOG_INFO);
	_mmu_trace(64, va);
	_mmu_trace(, ") = ");
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

static void mmu_set_mair(void) {

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

static void mmu_set_tcr(void) {

	reg_t tcr = 0;

	/* TTBR1 */
	tcr |= (64 - CONFIG_VA_BITS);
	tcr |= TCR_MISS_FAULT;
	tcr |= (TCR_CACHEABLE_WB_WA << 8);
	tcr |= (TCR_CACHEABLE_WB_WA << 10);
	tcr |= TCR_INNER_SHAREABLE;
	tcr <<= 16; /* it's important to keep these
		     * after the shift! */
	tcr |= (TCR_TOP_BYTE_USED << 1);
	tcr |= granule(1);

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

void map_kernel(void) {
	pentry_t* kentry;
	pentry_t* pte;
	unsigned int i;
	pentry_t curr_addr;

	i = 0;

	kentry = ptable_get_free_entry(ptable_get_gpt());

	ptable_init_from(ptable_get_gpt(), &kpt);

	/* map first GB for kernel */
	*kentry = PT_TABLE_DESC;
	*kentry |= (pentry_t)raw_ptr(kpt.raw_table);

	for_each_pte_in(&kpt, pte) {

		curr_addr = 0x0//((pentry_t)&_start)
			  + (pentry_t)(kpt.entry_span * (i++));

		if (curr_addr >= 0x3f000000) {
			break;
		}
		else {
			*pte = curr_addr | PE_KERNEL_CODE | PT_BLOCK_ENTRY;
		}
	}

	dump_table(&kpt);

	return;
}

void map_device(void) {
	pentry_t* dentry;
	pentry_t* pte;
	unsigned int i;
	pentry_t curr_addr;

	i = 0;

	dentry = ptable_get_free_entry(ptable_get_gpt());

	ptable_init_from(ptable_get_gpt(), &dpt);

	/* map device */
	*dentry |= PT_TABLE_DESC;
	*dentry |= (pentry_t)raw_ptr(dpt.raw_table);

	for_each_pte_in(&dpt, pte) {
		curr_addr = 0x3f000000 + (pentry_t)(dpt.entry_span * (i++));

		*pte = curr_addr | PE_DEVICE | PT_BLOCK_ENTRY;
	}

	dump_table(&dpt);

	return;
}

/* general */

ptable_t empty;

extern reg_t UARTBASE;

void mmu_init(void) {

	ptable_gpt_init(1024 * 1024 * 1024, 512);
	load_table("0", raw_ptr(ptable_get_gpt()->raw_table));

	ptable_init(&empty, 1024 * 1024 * 1024, 512);
	load_table("1", raw_ptr(empty.raw_table));

	map_kernel();
	map_device();

	mmu_set_mair();
	mmu_set_tcr();

	asm volatile(
		"dsb ish; isb; msr sctlr_el1, %0;"
		"isb; nop; nop; nop; nop"
		:
		:"r"(0x5 | (1 << 12))
	);

	UARTBASE = 0x40000000 + 0x215000;

	mmu_trace(, "GPT: ", LOG_INFO);
	_mmu_trace(ptr, ptable_get_gpt()->raw_table);
	_mmu_trace(ln, "");

#ifdef MMU_TRACE
	dump_table(ptable_get_gpt());
#endif

	mmu_trace(ln, "MMU initialized.", LOG_INFO);

	return;
}
