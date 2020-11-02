#include "config.h"

.section ".text.boot"

.globl _ld_heap

.globl _start
_start:
	/* if we are not on core0, hang */
	mrs	x0, mpidr_el1
	and	x0, x0, #0xFF
	cbz	x0, control
	b	hang

control:

	/* setup for U-Boot: if we are
	 * in EL2, skip EL3 entry */

	curr_el_to x0
	cmp	x0, #3
	b.eq	el3_entry
	cmp	x0, #2
	b.eq	no_el3_entry

	b hang

el3_entry:

	scr_el3 _UNSECURE _IRQ_NOROUTE _FIQ_NOROUTE

	/* MMU and cache is always
	 * disabled in EL3 */
	set_mmu_el 2 _MMU_DISABLED
	set_mmu_el 1 _MMU_DISABLED

	enter_el 3 2 el2_entry

no_el3_entry:

	set_mmu_el 2 _MMU_DISABLED
	set_mmu_el 1 _MMU_DISABLED

el2_entry:

	hcr_el2	_IRQ_NOROUTE _FIQ_NOROUTE

	enter_el 2 1 el1_entry

el1_entry:

	/* setup early stack */
	adr	x1, _ld_early_stack
	mov	sp, x1

	bl	uart_init

	irq_vector_el 1

	adr	x0, irqs_init_msg
	adr	x1, uart_write_char
	bl	print_string

	adr	x0, _ld_bss_start
	adr	x1, _ld_bss_end
	bl	memzero

	adr	x0, _ld_tt_l1_base
	adr	x1, _ld_tt_l1_base_end
	bl	memzero

	adr	x0, test_string
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
