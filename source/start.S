.section ".text.boot"

.globl _start
_start:
	/* if we are not on core0, hang */
	mrs	x0, mpidr_el1
	and	x0, x0, #0xFF
	cbz	x0, control
	b	hang

control:
	/* setup stack */
	adr	x1, stack
	mov	sp, x1

	bl	uart_init

	adr	x0, test_string
	adr	x1, uart_write_char
	bl	print_string

	ldr	x0, =0xABCDEF0123456789
	adr	x1, uart_write_char
	mov	x2, #8
	bl	print_hex

	ret

hang:
	wfe
	b	hang

.section ".data"

test_string: .asciz "\n\rHello.\n\rHow are you?\n\rI am under the water.\n\rPlease help me.\n\r"
