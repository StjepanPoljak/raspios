#include "config.h"

/* void */
.globl mmu_enable_el1
mmu_enable_el1:
	sub	sp, sp, #0x16
	str	x30, [sp]

	set_mmu_el 1 _MMU_ENABLED
	isb

	ldr	x30, [sp]
	add	sp, sp, #0x16
	ret

.globl memzero
/* x0 - start address
 * x1 - end address */
memzero:
	sub	sp, sp, #16
	str	x30, [sp]

memzero_loop:
	strb	wzr, [x0]
	add	x0, x0, #1
	cmp	x0, x1
	b.lt	memzero_loop

	ldr	x30, [sp]
	add	sp, sp, #16
	ret
