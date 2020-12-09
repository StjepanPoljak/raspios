#include <mmu/mmu.h>
#include <mmu/mair.h>
#include <mmu/tcr.h>
#include <mem.h>

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

/* PTE-related */

#define for_each_table_entry(table, entry, skip) \
	for (entry = table; entry < table + 512; entry += skip)

#define pentry_t uint64_t
#define ptable_t pentry_t*

void dump_table(ptable_t table) {
	//pentry_t* entry;

	dump_mem((void*)table, 512, 8);

//	for_each_table_entry(table, entry, 2) {
//		print64_raw(raw_ptr(entry));
//		print(": ");
//		print64_raw(*entry);
//		print(" ");
//		print64_raw(*(entry + 1));
//		newline();
//	}
}

extern void memzero(addr_t, addr_t);

ptable_t create_table(void) {
	ptable_t table;
	entry_t* entry;
	uint16_t count = 0;

	table = (ptable_t)alloc_fast_align(4096, 4096);

	for_each_table_entry(table, entry, 1) {
		*entry = count++;
	}

	//memzero(raw_ptr(table), raw_ptr(table) + 4096);

	return table;
}

/* 0 - 512GB, 1 - 1GB, 2 - 2MB, 3 - 4kB
 * level 0 can only point to a next level 1 entry
 * level 3 cannot point to another table and can only
 * output block address */

#define PT_BLOCK_ENTRY	0x1	/* 1, 2 */
#define PT_TABLE_ENTRY	0x3	/* 1, 2 */
#define PT_TABLE_DESC	0x3	/* 0, 1, 2 */
#define PT_INVALID	0x0	/* 0, 1, 2, 3 */

void set_entry(entry_t* table, uint16_t entry_no, entry_t entry) {
	return;
}

/*
static uint8_t mmu_access_permission(enum mmu_ap_unprivileged apu, enum mmu_ap_privileged app) {

	switch (apu) {
	case MMU_NO_ACCESS_EL0:
		return app == MMU_RW_EL123 ? 0x0 : 0x80;
	case MMU_RW_EL0:
		return 0x40;
	case MMU_RO_EL0:
		return 0xc0;
	default:
		break;
	}

	return 0x80;
}

static void mmu_set_tts1_attrs(reg_t* addr, uint8_t mair_index, uint8_t ns, uint8_t ap, enum mmu_sh_attr sh, enum mmu_ep ep) {

	*addr |= ((reg_t)mair_index << 2);
	*addr |= ((reg_t)ns << 5);
	*addr |= (reg_t)ap;
	*addr |= ((reg_t)sh << 8);
	*addr |= ((reg_t)ep << 53);

	return;
}
*/

/* general */

void mmu_init(void) {
	ptable_t table;

	mmu_trace(ln, "Initializing MMU.", LOG_INFO);

	mmu_set_mair();
	mmu_set_tcr();

	table = create_table();
	dump_table(table);

	//mmu_load_table("ttbr0_el1", "_ld_tt_l1_base");
	
	sync_all();

	return;
}
