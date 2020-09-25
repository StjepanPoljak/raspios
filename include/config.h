.macro hypervisor_el2 irqs
	/*	
	 * ======= hcr_el2 =======
	 *    hypervisor control
	 *        register
	 * ======================= 
	 */

	/* RW (31)
	 * -----------------------
	 * execution state for EL1
	 * is AArch64 */
	ldr	x0, =0x80000000

	/* physical IRQ routing (4)
	 * -----------------------
	 * IRQs are taken to EL2
	 * unless routed to EL3 */
	orr	x0, x0, #0x10

	msr	hcr_el2, x0
.endm

.macro enter_el el entry
	adr	x0, \entry
	msr	elr_el\el, x0

	eret
.endm

.macro sctlr_el el
	/*
	 * ===== sctlr_elx =======
	 *     system control
	 *        register 
	 * =======================
	 */

	/* RESERVED (29:28)
	 * RESERVED (23:22)
	 * RESERVED (18)
	 * RESERVED (16)
	 * RESERVED (11)
	 * RESERVED (5:4)
	 * -----------------------
	 * write 1 to each reserved
	 * register */
	ldr	w0, =0x30c50830

	/* I (12)
	 * -----------------------
	 * disable I-cache */

	/* C (2)
	 * -----------------------
	 * disable d-cache */

	/* M (0)
	 * -----------------------
	 *  disable MMU */

	/* to disable I, C and M,
	 * write zeros
	 * (already in w0) */

	msr	sctlr_el\el, x0

.endm

.macro hypervisor_el3

	sctlr_el 2

	hypervisor_el2

	/*	
	 * ======= scr_el3 =======
	 *  secure configuration
	 *        register
	 * ======================= 
	 */

	/* RW (10)
	 * -----------------------
	 * execution state for EL2
	 * is AArch64 */
	ldr	x0, =0x400

#if SECURE == 1
#error "Cannot configure hypervisor as secure."
#endif

	/* NS (0)
	 * -----------------------
	 * exception levels lower
	 * than EL3 are in non-secure
	 * state (note that there is
	 * no EL2 in secure world) */
	orr	x0, x0, #1
	msr	scr_el3, x0

	/*	
	 * ====== spsr_el3 =======
	 *  saved program status
	 *        register
	 * ======================= 
	 */

	/* M (3:0)
	 * -----------------------
	 * AArch64 exception level
	 * and selected stack pointer
	 * (here: EL2h) */
	mov	x0, #0x9
	msr	spsr_el3, x0
.endm

.macro kernel_el el

	sctlr_el \el

	/*	
	 * ======= hcr_el2 =======
	 *    hypervisor control
	 *        register
	 * ======================= 
	 */

	/* RW (31)
	 * -----------------------
	 * execution state for EL1
	 * is AArch64 */
	ldr	x0, =0x80000000
	msr	hcr_el2, x0
	
	/*	
	 * ====== spsr_elx =======
	 *  saved program status
	 *        register
	 * ======================= 
	 */

	/* M (3:0)
	 * -----------------------
	 * AArch64 exception level
	 * and selected stack pointer
	 * (here: EL1h) */
	mov	x0, #0x5
	msr	spsr_el\el, x0
.endm

/* secure = 1
 * non-secure = 0 */
.macro scr_el3 secure irqs
	/*	
	 * ======= scr_el3 =======
	 *  secure configuration
	 *        register
	 * ======================= 
	 */

	/* RW (10)
	 * -----------------------
	 * execution state for EL2
	 * is AArch64 */
	ldr	x0, =0x400

	/* IRQ (1)
	 * -----------------------
	 * take physical IRQs to
	 * EL3 (bit set) */

	ldr	x1, =\irqs
	lsl	x1, x1, #1
	and	x1, x1, #2
	orr	x0, x0, x1

	/* NS (0)
	 * -----------------------
	 * exception levels lower
	 * than EL3 are in non-secure
	 * state (note that there is
	 * no EL2 in secure world) */
	ldr	x1, =\secure
	and	x1, x1, #1
	orr	x0, x0, x1

	msr	scr_el3, x0
.endm

.macro curr_el_to reg
	mrs	\reg, CurrentEL
	lsr	\reg, \reg, #2
	and	\reg, \reg, #0xf
.endm

.macro irq_vector_el el
	adr	x0, irq_vector
	msr	vbar_el\el, x0
.endm

