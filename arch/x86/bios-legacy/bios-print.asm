; al <- char to print
print_char:
	push eax
	push ebx
	mov bl, al
	xor eax, eax
	mov al, bl
	xor ebx, ebx
	mov ah, 0x0E
	mov bh, 0x00
	mov bl, 0x07
	int 0x10
	pop ebx
	pop eax
	ret
	
; al <- digit to print	
print_digit:
	push eax
	push ebx
	mov bl, al
	xor eax, eax
	mov al, bl
	cmp al, 9
	ja .greater_than_10
	add al, 48
.print_digit_cont:
	call print_char
	jmp .print_digit_done
	
.greater_than_10:
	add al, 55
	jmp .print_digit_cont
.print_digit_done:
	pop ebx
	pop eax
	ret

; si <- pointer to string
print_string:
	push eax
	push ebx
	push si
.print_string_loop:
	lodsb
	test al, al
	jz .print_string_done
	call print_char
	jmp .print_string_loop
.print_string_done:
	pop si
	pop ebx
	pop eax
	ret

print_newline:
	push eax
	mov al, 13
	call print_char
	mov al, 10
	call print_char

	pop eax
	ret
	
; eax <- value to print
; ebx <- base
print_num:
	push eax
	push ebx
	push ecx
	push edx
	xor ecx, ecx
	xor edx, edx

.div_loop:
	xor edx, edx
	inc ecx
	div ebx
	push edx
	test eax, eax
	jz .print_digits_loop
	jmp .div_loop

.print_digits_loop:
	pop edx
	xor eax, eax
	mov al, dl
	call print_digit
	dec ecx
	test ecx, ecx
	jz .print_digits_done
	jmp .print_digits_loop

.print_digits_done:
	pop edx
	pop ecx
	pop ebx
	pop eax
	ret

; ax <- value to print
; bl <- prepend 0x
print_hex_raw:
	push eax
	push ebx

	test bl, bl
	jz .skip_prepend

	push eax
	mov al, 48
	call print_char
	mov al, 120
	call print_char	
	pop eax

.skip_prepend:
	mov ebx, 16
	call print_num
	pop ebx
	pop eax
	ret

print_hex:
	push eax
	push ebx
	xor ebx, ebx
	mov bl, 1
	call print_hex_raw
	pop ebx
	pop eax
	ret
