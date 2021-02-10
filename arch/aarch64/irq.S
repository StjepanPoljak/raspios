.macro regs_save
	sub sp, sp, #(16*16)
	stp x0, x1, [sp]
	stp x2, x3, [sp, #(16*1)]
	stp x4, x5, [sp, #(16*2)]
	stp x6, x7, [sp, #(16*3)]
	stp x8, x9, [sp, #(16*4)]
	stp x10, x11, [sp, #(16*5)]
	stp x12, x13, [sp, #(16*6)]
	stp x14, x15, [sp, #(16*7)]
	stp x16, x17, [sp, #(16*8)]
	stp x18, x19, [sp, #(16*9)]
	stp x20, x21, [sp, #(16*10)]
	stp x22, x23, [sp, #(16*11)]
	stp x24, x25, [sp, #(16*12)]
	stp x26, x27, [sp, #(16*13)]
	stp x28, x29, [sp, #(16*14)]
	str x30, [sp, #(16*15)]
.endm

.macro regs_restore
	ldp x0, x1, [sp]
	ldp x2, x3, [sp, #(16*1)]
	ldp x4, x5, [sp, #(16*2)]
	ldp x6, x7, [sp, #(16*3)]
	ldp x8, x9, [sp, #(16*4)]
	ldp x10, x11, [sp, #(16*5)]
	ldp x12, x13, [sp, #(16*6)]
	ldp x14, x15, [sp, #(16*7)]
	ldp x16, x17, [sp, #(16*8)]
	ldp x18, x19, [sp, #(16*9)]
	ldp x20, x21, [sp, #(16*10)]
	ldp x22, x23, [sp, #(16*11)]
	ldp x24, x25, [sp, #(16*12)]
	ldp x26, x27, [sp, #(16*13)]
	ldp x28, x29, [sp, #(16*14)]
	ldr x30, [sp, #(16*15)]
	add sp, sp, #(16*16)
.endm

.balign 4096
.globl irq_vector
irq_vector:
	/* sp_el0/thread mode */
	.balign 128
	b invalid_irq_handler	//synchronous
	.balign 128
	b invalid_irq_handler	//irq
	.balign 128
	b invalid_irq_handler	//fiq
	.balign 128
	b invalid_irq_handler	//error

	/* sp_elx/handler mode */
	.balign 128
	b invalid_irq_handler	//synchronous
	.balign 128
	b sp_elx_irq_handler	//irq
	.balign 128
	b invalid_irq_handler	//fiq
	.balign 128
	b invalid_irq_handler	//error

	/* aarch64 */
	.balign 128
	b invalid_irq_handler	//synchronous
	.balign 128
	b invalid_irq_handler	//irq
	.balign 128
	b invalid_irq_handler	//fiq
	.balign 128
	b invalid_irq_handler	//error

	/* aarch32 */
	.balign 128
	b invalid_irq_handler	//synchronous
	.balign 128
	b invalid_irq_handler	//irq
	.balign 128
	b invalid_irq_handler	//fiq
	.balign 128
	b invalid_irq_handler	//error

hang:
	b hang

invalid_irq_handler:
	regs_save
	mrs x0, esr_el1
	adr x1, uart_write_char
	mov x2, #8
	bl print_hex
1:
	b 1b

sp_elx_irq_handler:
	regs_save
	adr x0, irq_handler_message
	adr x1, uart_write_char
	bl print_string

/*
	ldr x0, =0x3f00b208
	ldr w0, [x0]
	adr x1, uart_write_char
	mov x2, #4
	bl print_hex

	ldr x0, =0x3f00b204
	ldr w0, [x0]
	adr x1, uart_write_char
	mov x2, #4
	bl print_hex_raw
*/	
	bl timer_init

	ldr x0, =0x40003000
	mov w1, #2
	str w1, [x0]

	regs_restore
	eret

invalid_irq_message: .asciz "(!) Unhandled IRQ received.\n\r"
irq_handler_message: .asciz "(i) Got IRQ.\n\r"
