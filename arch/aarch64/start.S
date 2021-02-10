#include "config.h"

.macro early_print addr f
	adr	x0, \addr
	adr	x1, \f
	bl	print_string
.endm

.macro early_print_addr addr f size
	adr	x0, \addr
	adr	x1, \f
	mov	x2, #\size
	bl	print_hex
.endm

.macro early_print_x0 f size
	adr	x1, \f
	mov	x2, #\size
	bl	print_hex
.endm

.macro disable_mmu
	msr sctlr_el2, xzr
	msr sctlr_el1, xzr
.endm

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
	disable_mmu

	enter_el 3 2 el2_entry

no_el3_entry:

	disable_mmu

el2_entry:

	hcr_el2	_IRQ_NOROUTE _FIQ_NOROUTE

	mrs	x0, midr_el1
	msr	vpidr_el2, x0

	mrs	x0, mpidr_el1
	msr	vmpidr_el2, x0

	msr	vttbr_el2, xzr

	enter_el 2 1 el1_entry

el1_entry:

	msr	daifset, #2

	/* setup early stack */
	adr	x1, _ld_stack
	mov	sp, x1

	adr	x0, _ld_bss_start
	adr	x1, _ld_bss_end
	bl	memzero

	bl	uart_init

	early_print test_string uart_write_char

	early_print paddr_msg uart_write_char
	adr	x0, _start
	early_print_x0 uart_write_char 8

	bl newline

	early_print stack_paddr_msg uart_write_char
	early_print_addr _ld_stack uart_write_char 8

	bl newline

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

.section .data

test_string:
	.asciz "\n\r\n\rHello.\n\rHow are you?\n\rI am under the water.\n\rPlease help me.\n\r\n\r"
curr_el_msg:
	.asciz " * CurrentEL: "
irqs_init_msg:
	.asciz "(i) IRQ vector loaded.\n\r"
paddr_msg:
	.asciz "(i) Loaded at physical address: "
stack_paddr_msg:
	.asciz "(i) Stack physical address: "

