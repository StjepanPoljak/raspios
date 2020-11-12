#ifndef MMU_H
#define MMU_H

/* common */

enum mmu_sh_attr {
	MMU_NON_SHAREABLE	= 0x0,
	MMU_OUTER_SHAREABLE	= 0x2,
	MMU_INNER_SHAREABLE	= 0x3
};

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

enum mmu_granule_size {
	MMU_GRANULE_4KB		= 0x0,
	MMU_GRANULE_64KB	= 0x1,
	MMU_GRANULE_16KB	= 0x2
};

enum mmu_ipa_size {
	MMU_IPA_32BIT		= 0x0,
	MMU_IPA_36BIT		= 0x1,
	MMU_IPA_40BIT		= 0x2,
	MMU_IPA_42BIT		= 0x3,
	MMU_IPA_44BIT		= 0x4,
	MMU_IPA_48BIT		= 0x5,
	MMU_IPA_52BIT		= 0x6
};

enum mmu_top_byte {
	MMU_TOP_BYTE_USED	= 0x0,
	MMU_TOP_BYTE_IGNORED	= 0x1
};

enum mmu_asid_ttbr {
	MMU_ASID_TTBR0		= 0x0,
	MMU_ASID_TTBR1		= 0x1
};

enum mmu_asid_size {
	MMU_ASID_8BIT		= 0x0,
	MMU_ASID_16BIT		= 0x1
};

/* Translation Table Entry Flags */

enum mmu_ap_unprivileged {
	MMU_NO_ACCESS_EL0,
	MMU_RW_EL0,
	MMU_RO_EL0
};

enum mmu_ap_privileged {
	MMU_RW_EL123,
	MMU_RO_EL123
};

enum mmu_ep {
	MMU_UXN			= 0x2,
	MMU_PXN			= 0x1,
	MMU_UXN_PXN		= 0x3,
	MMU_X			= 0x0
};

/* MMU functions */

void mmu_init(void);

#endif
