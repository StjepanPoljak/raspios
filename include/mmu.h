#ifndef MMU_H
#define MMU_H

/* MAIR-related enums */

enum mmu_device_attr {
	MMU_DEVICE_nGnRnE	= 0x0,
	MMU_DEVICE_nGnRE	= 0x4,
	MMU_DEVICE_nGRE		= 0x8,
	MMU_DEVICE_GRE		= 0xc
};

enum mmu_trans_attr {
	MMU_TRANSIENT		= 0x8,
	MMU_NON_TRANSIENT	= 0x0
};

enum mmu_write_policy {
	MMU_WRITE_BACK		= 0x4,
	MMU_WRITE_THROUGH	= 0x0
};

enum mmu_read_alloc {
	MMU_READ_ALLOCATE	= 0x2,
	MMU_READ_NO_ALLOCATE	= 0x0
};

enum mmu_write_alloc {
	MMU_WRITE_ALLOCATE	= 0x1,
	MMU_WRITE_NO_ALLOCATE	= 0x0
};

/* TCR-related enums */

/* t0 << 7 */
enum mmu_tlb_miss_attr {
	MMU_NO_FAULT		= 0x0,
	MMU_TRANSLATION_FAULT	= 0x1
};

enum mmu_tlb_sh_attr {
	MMU_NON_SHAREABLE	= 0x0,
	MMU_OUTER_SHAREABLE	= 0x2,
	MMU_INNER_SHAREABLE	= 0x3
};

enum mmu_granule_size {
	MMU_4KB			= 0x0,
	MMU_64KB		= 0x1,
	MMU_16KB		= 0x2
};

enum mmu_ipa_size {
	MMU_32BIT		= 0x0,
	MMU_36BIT		= 0x1,
	MMU_40BIT		= 0x2,
	MMU_42BIT		= 0x3,
	MMU_44BIT		= 0x4,
	MMU_48BIT		= 0x5,
	MMU_52BIT		= 0x6
};

/* MMU functions */

void mmu_init(void);

#endif
