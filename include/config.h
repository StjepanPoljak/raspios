.macro hypervisor_el3

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

/* secure = 1
 * non-secure = 0 */
.macro os_el3 secure

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

	/* NS (0)
	 * -----------------------
	 * exception levels lower
	 * than EL3 are in non-secure
	 * state (note that there is
	 * no EL2 in secure world) */
	ldr	x1, =\secure
	orr	x0, x0, x1

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
	 * (here: EL1h) */
	mov	x0, #0x5

	msr	spsr_el3, x0
.endm

.macro setup_irq_vector
	adr	x0, irq_vector
	mrs	x1, CurrentEL

	lsr	x1, x1, #2
	cmp	x1, #2
	b.eq	el2_irq_vbar
	cmp	x1, #1
	b.eq	el1_irq_vbar
	b	irq_vector_end

el2_irq_vbar:	
	msr	vbar_el2, x0
	b	irq_vector_end
el1_irq_vbar:
	msr	vbar_el1, x0
	b	irq_vector_end
irq_vector_end:
.endm


