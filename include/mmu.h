#ifndef MMU_H
#define MMU_H

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

void mmu_init(void);

#endif
