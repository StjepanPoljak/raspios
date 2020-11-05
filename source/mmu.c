#include "mmu.h"

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

#define mmu_load_table(TABLE_MSR, TABLE_BASE) \
do { \
	mmu_trace(ln, "Loading " TABLE_MSR "@" TABLE_BASE, LOG_INFO); \
	__asm volatile( \
		"adr x0, " TABLE_BASE ";" \
		"msr " TABLE_MSR ", x0"\
		); \
} while(0);

static uint8_t mmu_add_device_memory(uint8_t mair[8], uint8_t index,
				     enum mmu_device_attr dattr) {

	if (index >= 8 || index < 0) {
		log_error(, "Invalid MAIR index (");
		_log(64, index);
		_log(ln, ") passed to mmu_add_device_memory()");

		return 1;
	}

	mair[index] = (uint8_t)dattr;

	mmu_trace(, "Created device (", LOG_INFO);
	_mmu_trace(64, (uint8_t)dattr);
	_mmu_trace(ln, ").");

	return 0;
}

static uint8_t mmu_create_cacheable(enum mmu_trans_attr trans,
				    enum mmu_write_policy wp,
				    enum mmu_read_alloc ra,
				    enum mmu_write_alloc wa) {

	uint8_t res = trans | wp | ra | wa;

	mmu_trace(, "Created cacheable (", LOG_INFO);
	_mmu_trace(64, res);
	_mmu_trace(ln, ").");

	return res;
}

static uint8_t mmu_create_non_cacheable(void) {

	uint8_t res = 0x4;

	mmu_trace(, "Created non-cacheable (", LOG_INFO);
	_mmu_trace(64, res);
	_mmu_trace(ln, ").");

	return res;
}

static uint8_t mmu_add_normal_memory(uint8_t mair[8], uint8_t index,
				     uint8_t inner, uint8_t outer) {

	if (index >= 8 || index < 0) {
		log_error(, "Invalid MAIR index (");
		_log(64, index);
		_log(ln, ") passed to mmu_add_normal_memory()");

		return 1;
	}

	mair[index] = inner | (outer << 4);

	mmu_trace(, "Merged normal memory (", LOG_INFO);
	_mmu_trace(64, mair[index]);
	_mmu_trace(ln, ").");

	return 0;
}

static void mmu_save_mair(uint8_t mair[8]) {

#ifdef MMU_TRACE
	reg_t mair_reg = 0;
	uint8_t i = 0;
	for (i = 0; i < 8; i++)
		mair_reg |= (mair[i] << (i * 8));

	mmu_trace(, "Saving MAIR (", LOG_INFO);
	_mmu_trace(64, mair_reg);
	_mmu_trace(ln, ").");
#endif
	__asm volatile(
		"ldr x0, [%0];"
		"msr mair_el1, x0;"
		: // no output
		: "r" (mair));

	return;
}

/* TCR-related */

void mmu_invalidate_tlbs(void) {

	__asm volatile(
		"tlbi alle3;"
		"dsb sy;"
		"isb");

	return;
}

static uint8_t mmu_tlb_non_cacheable_attr(void) {

	return 0x0;
}

static uint8_t mmu_tlb_cacheable_attr(
		enum mmu_write_policy wp,
		enum mmu_write_alloc wa) {

	if (wp == MMU_WRITE_BACK && wa == MMU_WRITE_ALLOCATE)
		return 0x1;

	else if (wp == MMU_WRITE_THROUGH && wa == MMU_WRITE_NO_ALLOCATE)
		return 0x2;

	else if (wp == MMU_WRITE_BACK && wa == MMU_WRITE_NO_ALLOCATE)
		return 0x3;

	else {
		log_error(ln, "Invalid TLB cache attribute.");
		_mmu_trace(, "  -> wp=");
		_mmu_trace(64, wp);
		_mmu_trace(, " wa=");
		_mmu_trace(64, wa);
		_mmu_trace(ln, "");
	}

	return 0x0;
}

static uint8_t mmu_create_tcr_attrs(uint8_t va_space_bits,
				    enum mmu_tlb_miss_attr miss_attr,
				    uint8_t inner_attrs,
				    uint8_t outer_attrs,
				    enum mmu_tlb_sh_attr sh_attr,
				    enum mmu_granule_size granule_size) {
	uint32_t res = 0;

	if (va_space_bits <= 32) {
		log_error(, "Invalid VA space size (");
		_log(64, va_space_bits);
		_log(ln, ")");

		return 2;
	}

	if (inner_attrs >= 4) {
		log_error(, "Invalid IRGN bits (");
		_log(64, inner_attrs);
		_log(ln, ")");

		return 3;
	}

	if (outer_attrs >= 4) {
		log_error(, "Invalid ORGN bits (");
		_log(64, outer_attrs);
		_log(ln, ")");

		return 4;
	}

	res |= (64 - va_space_bits);
	res |= (miss_attr << 7);
	res |= (inner_attrs << 8);
	res |= (outer_attrs << 10);
	res |= (sh_attr << 12);
	res |= (granule_size << 14);

	mmu_trace(, "Created TCR table attribute: ", LOG_INFO);
	_mmu_trace(64, res);
	_mmu_trace(ln, "");

	return res;
}

static reg_t mmu_create_tcr(uint16_t t0, uint16_t t1,
			    enum mmu_ipa_size ipa_size) {

	reg_t res = (reg_t)t0 | ((reg_t)t1 << 16) | ((reg_t)ipa_size << 32);

	mmu_trace(, "Created TCR: ", LOG_INFO);
	_mmu_trace(64, res);
	_mmu_trace(ln, "");

	return res;
}

static uint8_t mmu_is_valid_table(uint8_t table) {

	if (table != 1 && table != 0) {
		log_error(, "Invalid table selected (");
		_log(64, table);
		_log(ln, ").");

		return FALSE;
	}

	return TRUE;
}

static uint8_t mmu_set_top_byte(reg_t* tcr, uint8_t table,
				enum mmu_top_byte top_byte) {

	uint8_t tb_pos;

	if (!mmu_is_valid_table(table))
		return 1;

	tb_pos = table == 0 ? 37 : 38;

	*tcr &= ~((reg_t)1 << tb_pos);
	*tcr |= (reg_t)1 << tb_pos;

	return 0;
}

static void mmu_set_asid(reg_t* tcr,
			 enum mmu_asid_ttbr asid_ttbr,
			 enum mmu_asid_size asid_size) {

	*tcr &= ~((reg_t)1 << 36);
	*tcr |= (reg_t)asid_size << 36;

	*tcr &= ~((reg_t)1 << 22);
	*tcr |= (reg_t)asid_ttbr << 22;

	return;
}

static void mmu_save_tcr(reg_t tcr) {

	__asm volatile(
		"msr tcr_el1, %0;"
		: // no output
		: "r" (tcr)
	);

	mmu_trace(, "Saved TCR (", LOG_INFO);
	_mmu_trace(64, tcr);
	_mmu_trace(ln, ").");

	return;
}

/* general */

void mmu_init(void) {

	uint8_t mair[8] = { 0 };
	reg_t tcr;

	mmu_trace(ln, "Initializing MMU.", LOG_INFO);

	mmu_add_normal_memory(mair, 0,
		mmu_create_cacheable(		// inner
			MMU_TRANSIENT,
			MMU_WRITE_BACK,
			MMU_READ_ALLOCATE,
			MMU_WRITE_ALLOCATE),
		mmu_create_cacheable(		// outer
			MMU_TRANSIENT,
			MMU_WRITE_BACK,
			MMU_READ_ALLOCATE,
			MMU_WRITE_ALLOCATE)
	);

	mmu_add_normal_memory(mair, 1,
		mmu_create_non_cacheable(),	// inner
		mmu_create_non_cacheable()	// outer
	);

	mmu_add_device_memory(mair, 2, MMU_DEVICE_nGnRnE);

	mmu_save_mair(mair);

	tcr = mmu_create_tcr(
		mmu_create_tcr_attrs(		// t0
			39,
			MMU_TRANSLATION_FAULT,
			mmu_tlb_cacheable_attr(
				MMU_WRITE_BACK,
				MMU_WRITE_ALLOCATE),
			mmu_tlb_cacheable_attr(
				MMU_WRITE_BACK,
				MMU_WRITE_ALLOCATE),
			MMU_INNER_SHAREABLE,
			MMU_GRANULE_4KB),
		mmu_create_tcr_attrs(		// t1
			39,
			MMU_TRANSLATION_FAULT,
			mmu_tlb_cacheable_attr(
				MMU_WRITE_BACK,
				MMU_WRITE_ALLOCATE),
			mmu_tlb_cacheable_attr(
				MMU_WRITE_BACK,
				MMU_WRITE_ALLOCATE),
			MMU_INNER_SHAREABLE,
			MMU_GRANULE_4KB),
		MMU_IPA_32BIT
	);

	mmu_set_top_byte(&tcr, 0, MMU_TOP_BYTE_IGNORED);
	mmu_set_top_byte(&tcr, 1, MMU_TOP_BYTE_IGNORED);

	mmu_set_asid(&tcr, MMU_ASID_TTBR1, MMU_ASID_8BIT);

	mmu_save_tcr(tcr);

	mmu_load_table("ttbr0_el1", "_ld_tt_l1_base");

	return;
}
