#ifndef MAIR_H
#define MAIR_H

/* ========== MAIR settings ==========*/

#define __MAIR_TRANSIENT		0x8
#define __MAIR_NON_TRANSIENT		0x0

#define __MAIR_WRITE_BACK		0x4
#define __MAIR_WRITE_THROUGH		0x0

#define __MAIR_READ_ALLOCATE		0x2
#define __MAIR_READ_NO_ALLOCATE		0x0

#define __MAIR_WRITE_ALLOCATE		0x1
#define __MAIR_WRITE_NO_ALLOCATE	0x0

#define _MAIR_DEVICE_nGnRnE		0x0
#define _MAIR_DEVICE_nGnRE		0x4
#define _MAIR_DEVICE_nGRE		0x8
#define _MAIR_DEVICE_GRE		0xc

#define _MAIR_NON_CACHEABLE		0x4

#define _MAIR_CACHEABLE 	(reg_t)(__MAIR_TRANSIENT | \
					__MAIR_WRITE_BACK | \
					__MAIR_READ_ALLOCATE | \
					__MAIR_WRITE_ALLOCATE)

/* ========== MAIR templates ==========*/

/* inner cacheable, outer cacheable */
#define MAIR_DEVICE		_MAIR_DEVICE_nGnRnE

/* inner cacheable, outer cacheable */
#define MAIR_CACHEABLE		(_MAIR_CACHEABLE | \
				(_MAIR_CACHEABLE << 4))

/* inner non-cacheable, outer non-cacheable */
#define MAIR_NON_CACHEABLE	(_MAIR_NON_CACHEABLE | \
				(_MAIR_NON_CACHEABLE << 4))

#define MAIR_DEVICE_INDEX		0
#define MAIR_CACHEABLE_INDEX		1
#define MAIR_NON_CACHEABLE_INDEX	2

#endif
