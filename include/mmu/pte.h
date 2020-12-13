#ifndef PTE_H
#define PTE_H

/* PTE-related */

/* 0 - 512GB, 1 - 1GB, 2 - 2MB, 3 - 4kB
 * level 0 can only point to a next level 1 entry
 * level 3 cannot point to another table and can only
 * output block address */

#define PT_BLOCK_ENTRY	((pentry_t)0x1)		/* 1, 2 */
#define PT_TABLE_ENTRY	((pentry_t)0x3)		/* 1, 2 */
#define PT_TABLE_DESC	((pentry_t)0x3)		/* 0, 1, 2 - points to next table */
#define PT_INVALID	((pentry_t)0x0)		/* 0, 1, 2, 3 */

#define PE_SECURE	((pentry_t)1 << 5)

#define PE_KERNEL_RO	((pentry_t)2 << 6)
#define PE_USER_RO	((pentry_t)3 << 6)
#define PE_KERNEL_RW	((pentry_t)0 << 6)
#define PE_USER_RW	((pentry_t)1 << 6)

#define PE_NSH		((pentry_t)0 << 8)
#define PE_OSH		((pentry_t)2 << 8)
#define PE_ISH		((pentry_t)3 << 8)

#define PE_ACCESSED	((pentry_t)1 << 10)
#define PE_CLEAR_AF	((pentry_t)0 << 10)

#define PE_PXN		((pentry_t)1 << 53)
#define PE_UXN		((pentry_t)1 << 54)

#define PE_DEVICE	(((pentry_t)MAIR_DEVICE_INDEX << 2) \
			| PE_KERNEL_RW | PE_PXN | PE_UXN | PE_OSH) \
			| PE_ACCESSED
#define PE_KERNEL_DATA	(((pentry_t)MAIR_CACHEABLE_INDEX << 2) \
			| PE_KERNEL_RW | PE_PXN | PE_UXN | PE_ISH) \
			| PE_ACCESSED
#define PE_KERNEL_CODE	(((pentry_t)MAIR_CACHEABLE_INDEX << 2) \
			| PE_KERNEL_RW | PE_UXN | PE_ISH) \
			| PE_ACCESSED
#define PE_USER_DATA	(((pentry_t)MAIR_CACHEABLE_INDEX << 2) \
			| PE_USER_RW | PE_PXN | PE_UXN | PE_ISH) \
			| PE_ACCESSED
#define PE_USER_CODE	(((pentry_t)MAIR_CACHEABLE_INDEX << 2) \
			| PE_USER_RW | PE_PXN | PE_ISH) \
			| PE_ACCESSED

#define pte_clear_mair(entry) (*entry &= ~((pentry_t)0x1c))
#define pte_mark_non_cacheable(entry) do { \
	pte_clear_mair(entry); \
	*entry |= (pentry_t)MAIR_NON_CACHEABLE_INDEX << 2; \
}
#define pte_clear_af(entry) (*entry &= ~((pentry_t)1 << 10))
#define pte_clear_pxn(entry) (*entry &= ~((pentry_t)1 << 53))
#define pte_clear_uxn(entry) (*entry &= ~((pentry)1 << 54))
#define pte_clear_sh(entry) (*entry &= ~((pentry_t)3 << 8))
#define pte_clear_ap(entry) (*entry &= ~((pentry_t)3 << 6))
#define pte_clear_type(entry) (*entry &= ~((pentry_t)3))

#define pte_get_phys(entry) (*entry & ~((((pentry_t)1 << 12) - 1) \
			  | ((((pentry_t)1 << 6) - 1) << 53)))

#endif
