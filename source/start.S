.section ".text.boot"

.globl _start
_start:
	/* if we are not on core0, hang */
	mrs	x0, mpidr_el1
	and	x0, x0, #0xFF
	cbz	x0, control
	b	hang

control:

	ldr x0, =0x30d00800
	msr sctlr_el1, x0

	ldr x0, =0x80000000
	msr hcr_el2, x0

	ldr x0, =0x431
	msr scr_el3, x0

	ldr x0, =0x1c5
	msr spsr_el3, x0

	adr x0, entry
	msr elr_el3, x0

	eret

entry:
	/* setup stack */
	adr	x1, stack
	mov	sp, x1

	bl	uart_init

	adr	x0, test_string
	adr	x1, uart_write_char
	bl	print_string

	adr	x0, irq_vector
	msr	vbar_el1, x0

	adr	x0, irqs_init_msg
	adr	x1, uart_write_char
	bl	print_string

	bl	timer_init

	mov	w0, #0x2
	ldr	x1, =0x3f00B210
	str	w0, [x1]

	ldr	w0, =0x2000000
	ldr	x1, =0x3f00B214
	str	w0, [x1]

	msr	daifclr, #2

	adr	x0, time_init_msg
	adr	x1, uart_write_char
	bl	print_string

	adr	x0, curr_el_msg
	adr	x1, uart_write_char
	bl	print_string

	mrs	x0, CurrentEL
	adr	x1, uart_write_char
	mov	x2, #8
	bl	print_hex

hang:
	wfi
	b	hang

.section ".data"

test_string: .asciz "\n\r\n\rHello.\n\rHow are you?\n\rI am under the water.\n\rPlease help me.\n\r\n\r\0"

curr_el_msg: .asciz " * CurrentEL: \0"
irqs_init_msg: .asciz "(i) IRQ vector loaded.\n\r\0"
time_init_msg: .asciz "(i) Timer initialized.\n\r\0"
