#ifndef TCR_H
#define TCR_H

#define TCR_MISS_NO_FAULT	((reg_t)0x0 << 7)
#define TCR_MISS_FAULT		((reg_t)0x1 << 7)

#define TCR_TT0_GRANULE_4KB	((reg_t)0x0 << 14)
#define TCR_TT0_GRANULE_64KB	((reg_t)0x1 << 14)
#define TCR_TT0_GRANULE_16KB	((reg_t)0x2 << 14)

#define TCR_TT1_GRANULE_16KB	((reg_t)0x1 << 30)
#define TCR_TT1_GRANULE_4KB	((reg_t)0x2 << 30)
#define TCR_TT1_GRANULE_64KB	((reg_t)0x3 << 30)

#define TCR_NON_SHAREABLE	((reg_t)0x0 << 12)
#define TCR_OUTER_SHAREABLE	((reg_t)0x2 << 12)
#define TCR_INNER_SHAREABLE	((reg_t)0x3 << 12)

#define TCR_NON_CACHEABLE	(reg_t)0x0
#define TCR_CACHEABLE_WB_WA	(reg_t)0x1
#define TCR_CACHEABLE_WT_NWA	(reg_t)0x2
#define TCR_CACHEABLE_WB_NWA	(reg_t)0x3

#define TCR_IPA_32BIT		((reg_t)0x0 << 32)
#define TCR_IPA_36BIT		((reg_t)0x1 << 32)
#define TCR_IPA_40BIT		((reg_t)0x2 << 32)
#define TCR_IPA_42BIT		((reg_t)0x3 << 32)
#define TCR_IPA_44BIT		((reg_t)0x4 << 32)
#define TCR_IPA_48BIT		((reg_t)0x5 << 32)
#define TCR_IPA_52BIT		((reg_t)0x6 << 32)

#define TCR_TOP_BYTE_USED	((reg_t)0x0 << 37)
#define TCR_TOP_BYTE_IGNORED	((reg_t)0x1 << 37)

#define TCR_ASID_TTBR0		((reg_t)0x0 << 22)
#define TCR_ASID_TTBR1		((reg_t)0x1 << 22)

#define TCR_ASID_8BIT		((reg_t)0x0 << 36)
#define TCR_ASID_16BIT		((reg_t)0x1 << 36)

#endif
