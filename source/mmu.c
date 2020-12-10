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

#define mmu_load_table(TABLE_MSR, TABLE_BASE) \
do { \
	mmu_trace(ln, "Loading " TABLE_MSR "@" TABLE_BASE, LOG_INFO); \
	__asm volatile( \
		"adr x0, " TABLE_BASE ";" \
		"msr " TABLE_MSR ", x0"\
		); \
} while(0);

ptable_t kpt;
ptable_t dpt;

static void mmu_set_mair(void) {

	reg_t mair = (MAIR_DEVICE << (MAIR_DEVICE_INDEX * 8)) |
		     (MAIR_CACHEABLE << (MAIR_CACHEABLE_INDEX * 8)) |
		     (MAIR_NON_CACHEABLE << (MAIR_NON_CACHEABLE_INDEX * 8));

	mmu_trace(, "Saving MAIR (", LOG_INFO);
	_mmu_trace(64, mair);
	_mmu_trace(ln, ").");
	
	__asm volatile(
		"ldr x0, [%0];"
		"msr mair_el1, x0;"
		: // no output
		: "r" (mair));

	return;
}

/* TCR-related */

static void mmu_set_tcr(void) {

	reg_t tcr = 0;

	/* kernel-space */
	tcr |= (64 - CONFIG_VA_BITS);
	tcr |= TCR_MISS_NO_FAULT;
	tcr |= (TCR_CACHEABLE_WB_WA << 8);
	tcr |= (TCR_CACHEABLE_WB_WA << 10);
	tcr |= TCR_INNER_SHAREABLE;
	tcr <<= 16; /* it's important to keep these
		     * after the shift! */
	tcr |= (TCR_TOP_BYTE_USED << 1);
	tcr |= granule(1);


	/* user-space */
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

	__asm volatile(
		"msr tcr_el1, %0;"
		"isb;"
		: // no output
		: "r" (tcr)
	);

	invalidate_tlbs_el(1);

	mmu_trace(, "Set TCR: ", LOG_INFO);
	_mmu_trace(64, tcr);
	_mmu_trace(ln, "");

}

extern int _start;

void map_kernel(void) {
	pentry_t* kentry;
	pentry_t* pte;
	unsigned int i;
	pentry_t curr_addr;

	i = 0;

	kentry = ptable_get_free_entry(ptable_get_gpt());

	ptable_init_from(ptable_get_gpt(), &kpt);

	/* map first GB for kernel */
	//*kentry = PE_KERNEL_CODE;
	*kentry = PT_TABLE_DESC;
	*kentry |= (pentry_t)(kpt.raw_table);

	for_each_pte_in(&kpt, pte) {

		curr_addr = ((pentry_t)&_start)
			  + (pentry_t)(kpt.entry_span * (i++));

		if (curr_addr >= 0x3f000000)
			break;

		*pte = curr_addr | PE_KERNEL_CODE | PT_BLOCK_ENTRY;
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
	//*kentry = PE_DEVICE;
	*dentry |= PT_TABLE_DESC;
	*dentry |= (pentry_t)(dpt.raw_table);

	for_each_pte_in(&dpt, pte) {
		curr_addr = 0x3f000000 + (pentry_t)(dpt.entry_span * (i++));

		if (curr_addr >= 0x40000000)
			break;

		*pte = curr_addr | PE_DEVICE | PT_BLOCK_ENTRY;
	}

	dump_table(&dpt);

	return;
}

/* general */

void mmu_init(void) {

	ptable_gpt_init(1024 * 1024 * 1024, 512);

	mmu_trace(ln, "Initialized GPT.", LOG_INFO);

	map_kernel();

	map_device();

	dump_table(ptable_get_gpt());

	mmu_trace(ln, "Initializing MMU.", LOG_INFO);

	mmu_set_mair();
	mmu_set_tcr();

	//mmu_load_table("ttbr0_el1", "_ld_tt_l1_base");
	
	sync_all();

	return;
}
