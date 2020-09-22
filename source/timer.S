.equ	TIMER_CS,	0x00
.equ	TIMER_CLO,	0x04
.equ	TIMER_CHI,	0x08
.equ	TIMER_C0,	0x0C
.equ	TIMER_C1,	0x10
.equ	TIMER_C2,	0x14
.equ	TIMER_C3,	0x18

.equ	TIMER_BASE,	0x3f003000

.globl timer_init
timer_init:
	sub	sp, sp, #(16*1)
	str	x30, [sp]

	ldr	x1, =TIMER_BASE
	ldr	w0, [x1, #TIMER_CLO]
	ldr	w2, =2000000
	add	w0, w0, w2
	str	w0, [x1, #TIMER_C1]

	ldr	x30, [sp]
	add	sp, sp, #(16*1)

	ret
