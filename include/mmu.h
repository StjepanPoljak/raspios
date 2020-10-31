#ifndef MMU_H
#define MMU_H

#define _MMU_DEVICE_nGnRnE 0x0
#define _MMU_DEVICE_nGnRE 0x4
#define _MMU_DEVICE_nGRE 0x8
#define _MMU_DEVICE_GRE 0xc

/* adds device memory to x0 */
.macro mmu_add_device_memory attr index
	mov	x1, #\attr
	mov	x2, #\index
	mul	x2, x2, #0x8
	lsl	x1, x1, x2
	orr	x0, x1, x1
.endm

#define _MMU_NON_TRANSIENT 0x8
#define _MMU_TRANSIENT 0x0

#define _MMU_WRITE_THROUGH 0x0
#define _MMU_WRITE_BACK 0x4

#define _MMU_READ_ALLOCATE 0x2
#define _MMU_READ_NO_ALLOCATE 0x0

#define _MMU_WRITE_ALLOCATE 0x1
#define _MMU_WRITE_NO_ALLOCATE 0x0

/* result to x1 */
.macro mmu_create_inner_cacheable_x1 transience write_policy read_alloc write_alloc
	mov	x1, #\transience
	orr	x1, x1, #\write_policy
	orr	x1, x1, #\read_alloc
	orr	x1, x1, #\write_alloc
.endm

/* result to x2 */
.macro mmu_create_outer_cacheable_x2 transience write_policy read_alloc write_alloc
	mov	x2, #\transience
	orr	x2, x2, #\write_policy
	orr	x2, x2, #\read_alloc
	orr	x2, x2, #\write_alloc
	lsl	x2, x2, #0x4
.endm

.macro mmu_create_inner_non_cacheable_x1 
	mov	x1, #0x4
.endm

.macro mmu_create_outer_non_cacheable_x2
	mov	x2, #0x4
	lsl	x2, x2, #0x4
.endm

/* x1 - inner
 * x2 - outer
 * x0 - result */
.macro mmu_add_normal_memory index
	mov	x0, x1
	orr	x0, x0, x2
	mov	x1, #\index
	mul	x1, x1, #0x8
	lsl	x0, x0, x1
.endm

#endif
