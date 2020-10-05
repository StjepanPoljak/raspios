#include "config.h"

.section ".text.boot"

.globl _start
_start:
	/* if we are not on core0, hang */
	mrs	x0, mpidr_el1
	and	x0, x0, #0xFF
	cbz	x0, control
	b	hang

control:

	/* setup for U-Boot: if we are
	 * in EL2, skip configuration
	 * but route IRQs */

	curr_el_to x0
	cmp	x0, #3
	b.eq	started_in_el3
	cmp	x0, #2
	b.eq	started_in_el2

	b hang

started_in_el3:

#if TARGET_EL == 3
	scr_el3 SECURE 1
	b entry
#elif TARGET_EL == 2
	hypervisor_el3
#elif TARGET_EL == 1
	kernel_el 3
	scr_el3 SECURE 0
#endif
	enter_el 3 entry

started_in_el2:

#if TARGET_EL == 2
	hypervisor_el2
	b entry
#elif TARGET_EL == 1
	kernel_el 2
#endif
	enter_el 2 entry

entry:
	/* setup stack */
	ldr	x1, =STACK_TOP
	mov	sp, x1

	bl	uart_init

	adr	x0, test_string
	adr	x1, uart_write_char
	bl	print_string

	irq_vector_el TARGET_EL

	adr	x0, irqs_init_msg
	adr	x1, uart_write_char
	bl	print_string

	bl	timer_init

	/* rpi enable timer IRQ */

	mov	w0, #0x2
	ldr	x1, =0x3f00B210
	str	w0, [x1]

	ldr	w0, =0x2000000
	ldr	x1, =0x3f00B214
	str	w0, [x1]

	/* enable interrupts */
	msr	daifclr, #2

	adr	x0, time_init_msg
	adr	x1, uart_write_char
	bl	print_string

	adr	x0, curr_el_msg
	adr	x1, uart_write_char
	bl	print_string

	curr_el_to x0
	adr	x1, uart_write_char
	mov	x2, #1
	bl	print_hex_raw

	bl	newline
	bl	newline

	bl	main

hang:
	wfi
	b	hang

.section ".data"

test_string: .asciz "\n\r\n\rHello.\n\rHow are you?\n\rI am under the water.\n\rPlease help me.\n\r\n\r\0"

curr_el_msg: .asciz " * CurrentEL: \0"
irqs_init_msg: .asciz "(i) IRQ vector loaded.\n\r\0"
time_init_msg: .asciz "(i) Timer initialized.\n\r\0"
