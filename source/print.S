/* x0 - register containing the half-byte to print (0xF mask) */
/* x1 - address of function to use to print char */
print_hex_4:
	sub sp, sp, #16
	stp x19, x30, [sp]

	mov x19, x0

	cmp x19, #10
	blt less_than_10

	add x0, x19, #55
	blr x1
	b ret_from_print_hex_4

less_than_10:

	add x0, x19, #48
	blr x1

ret_from_print_hex_4:

	ldp x19, x30, [sp]
	add sp, sp, #16

	ret

.globl print_hex_b
/* x0 - register containing the byte to print (0xFF mask) */
/* x1 - address of function to use to print */
print_hex_b:
	sub sp, sp, #16
	stp x19, x30, [sp]

	mov x19, x0

	lsr x0, x0, #4
	bl print_hex_4

	and x0, x19, 0xF
	bl print_hex_4

	ldp x19, x30, [sp]
	add sp, sp, #16

	ret

.macro switch first second placeholder
	mov \placeholder, \first
	mov \first, \second
	mov \second, \placeholder
.endm

.globl print_hex_raw
/* x0 - register to print
 * x1 - address of function to use to print
 * x2 - size in bytes */
print_hex_raw:
	sub sp, sp, #(16*2)
	str x19, [sp, #16]
	stp x20, x30, [sp]

print_hex_raw_loop:

	sub x2, x2, #1

	mov x19, #8
	mul x20, x2, x19

	mov x19, x0

	lsr x0, x0, x20
	and x0, x0, 0xFF
	bl print_hex_b

	mov x0, x19

	cbnz x2, print_hex_raw_loop

	ldp x20, x30, [sp]
	ldr x19, [sp, #16]
	add sp, sp, #(16*2)

	ret

.globl print_hex
/* x0 - register to print
 * x1 - address of function to use to print
 * x2 - size in bytes */
print_hex:
	sub sp, sp, #16
	stp x19, x30, [sp]

	mov x19, x0

	mov x0, #48
	blr x1

	mov x0, #120
	blr x1

	mov x0, x19

	bl print_hex_raw

	ldp x19, x30, [sp]
	add sp, sp, #16

	ret

.globl print_string
/* x0 - address of string
   x1 - address of function to print char */
print_string:
	sub	sp, sp, #16
	stp	x19, x30, [sp]

	mov	x19, x0

print_string_loop:
	ldrb	w0, [x19]
	and	w0, w0, #0xFF
	blr	x1

	add	x19, x19, #1
	cbnz	w0, print_string_loop

	ldp	x19, x30, [sp]
	add	sp, sp, #16

	ret

