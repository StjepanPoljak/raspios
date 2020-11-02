#include "mmu.h"

#include <stdint.h>

static uint8_t mmu_add_mair_entry(uint8_t mmu_mair[8], mmu_attr_entry_t* mmu_entry, uint8_t index) {

	struct mmu_cacheable_mem* attrs;

	if (index >= 8 || index < 0)
		return 1;

	mmu_mair[index] = 0x0;

	if (mmu_entry->mmu_type == MMU_DEVICE) {
		mmu_mair[index] = (uint8_t)mmu_entry->mmu_attr.device_attr;
		return 0;
	}

	if (mmu_entry->mmu_attr.normal_attrs.inner.cacheability == MMU_NON_CACHEABLE)
		mmu_mair[index] = 0x4;
	else {
		attrs = &(mmu_entry->mmu_attr.normal_attrs.inner.attrs);
		mmu_mair[index] = attrs->trans_attr
				| attrs->write_policy
				| attrs->read_allocate
				| attrs->write_allocate;
	}

	if (mmu_entry->mmu_attr.normal_attrs.outer.cacheability == MMU_NON_CACHEABLE)
		mmu_mair[index] |= 0x4 << 4;
	else {
		attrs = &(mmu_entry->mmu_attr.normal_attrs.outer.attrs);
		mmu_mair[index] = (attrs->trans_attr << 4)
				| (attrs->write_policy << 4)
				| (attrs->read_allocate << 4)
				| (attrs->write_allocate << 4);
	}

	return 0;
}
