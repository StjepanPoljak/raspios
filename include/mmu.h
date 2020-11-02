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

enum mmu_read_allocate {
	MMU_READ_ALLOCATE	= 0x2,
	MMU_READ_NO_ALLOCATE	= 0x0
};

enum mmu_write_allocate {
	MMU_WRITE_ALLOCATE	= 0x1,
	MMU_WRITE_NO_ALLOCATE	= 0x0
};

enum mmu_cacheability {
	MMU_CACHEABLE,
	MMU_NON_CACHEABLE
};

enum mmu_type {
	MMU_DEVICE,
	MMU_NORMAL
};

struct mmu_cacheable_mem {
	enum mmu_trans_attr trans_attr;
	enum mmu_write_policy write_policy;
	enum mmu_read_allocate read_allocate;
	enum mmu_write_allocate write_allocate;
};

struct mmu_normal_mem {
	enum mmu_cacheability cacheability;
	struct mmu_cacheable_mem attrs;
};

struct mmu_normal_mem_io {
	struct mmu_normal_mem inner;
	struct mmu_normal_mem outer;
};

union mmu_attr {
	struct mmu_normal_mem_io normal_attrs;
	enum mmu_device_attr device_attr;
};

typedef struct {
       enum mmu_type mmu_type;
       union mmu_attr mmu_attr;
} mmu_attr_entry_t;

#endif
