#define _SECURE 1
#define _UNSECURE 0

#define _IRQ_ROUTE 1
#define _IRQ_NOROUTE 0

#define _FIQ_ROUTE 1
#define _FIQ_NOROUTE 0

.macro hcr_el2 irq_route fiq_route
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
	mov	x1, #\irq_route
	lsl	x1, x1, #4
	orr	x0, x0, x1

	/* physical FIQ routing(3)
	 * -----------------------
	 * FIQs are taken to EL2
	 * unless routed to EL3 */
	mov	x1, #\fiq_route
	lsl	x1, x1, #3
	orr	x0, x0, x1

	msr	hcr_el2, x0
.endm


.macro enter_el el_src el_dest entry

	/* invalid for EL0
	 * EL1 = 0x5
	 * EL2 = 0x9 */
	mov	x0, #\el_dest
	lsl	x0, x0, #2
	add	x0, x0, #1
	msr	spsr_el\el_src, x0

	adr	x0, \entry
	msr	elr_el\el_src, x0

	eret
.endm

/* secure = 1
 * non-secure = 0 */
.macro scr_el3 secure irq_route fiq_route
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

	/* HCE (8)
	 * -----------------------
	 * enable HVC instructions */
	mov	x1, #1
	lsl	x1, x1, #8
	orr	x0, x1, x1

	/* EA (3)
	 * -----------------------
	 * route SError to EL3 */
	orr	x0, x0, #0x8

	/* FIQ (2)
	 * -----------------------
	 * take physical FIQs to
	 * EL3 */
	mov	x1, #\fiq_route
	lsl	x1, x1, #2
	orr	x0, x0, x1

	/* IRQ (1)
	 * -----------------------
	 * take physical IRQs to
	 * EL3 */
	mov	x1, #\irq_route
	lsl	x1, x1, #1
	orr	x0, x0, x1

	/* NS (0)
	 * -----------------------
	 * exception levels lower
	 * than EL3 are in non-secure
	 * state (note that there is
	 * no EL2 in secure world) */
	mov	x1, #\secure
	orr	x0, x0, x1

	msr	scr_el3, x0
.endm

.macro curr_el_to reg
	mrs	\reg, CurrentEL
	lsr	\reg, \reg, #2
	and	\reg, \reg, #0xf
.endm
