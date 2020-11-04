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

static uint8_t mmu_create_cacheable(
		enum mmu_trans_attr trans,
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

void mmu_init(void) {

	uint8_t mair[8] = { 0 };

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

	mmu_load_table("ttbr0_el1", "_ld_tt_l1_base");
}
