bits 16
	
org 0x8000

start:
	mov [boot_drive], al
	mov [sector_count], ah

	mov sp, 0x8000

	mov si, boot_drive_msg
	call print_string

	xor ax, ax
	mov al, [boot_drive]
	call print_hex

	call print_newline

	mov si, sector_count_msg
	call print_string

	xor ax, ax
	mov al, [sector_count]
	mov bx, 0x10
	call print_num
	call print_newline

	call memory_map

	call print_newline

skip:	
	call print_memory_map
	
	cli
	lgdt [gdt_descriptor]

	mov eax, cr0
	or eax, 1
	mov cr0, eax

	;; segment selector
	;; ---------------------------------------------------------------
	;; field				bits	comment
	;; ---------------------------------------------------------------
	;; RPL (Requested Privilege Level)	0:1	(0 kernel, 3 user)
	;; TI (Table Indicator)			2	(0 GDT, 1 LDT)
	;; GDT index				3:15
	;; ---------------------------------------------------------------
	
	;; we set CS to point to 0x08 (gdt_code entry)
	jmp 0x08:protected_mode_start

%include "bios-print.asm"

memory_map:
	push eax
	push ebx
	push ecx
	push edx
	push di
	push si

	mov eax, 0x4000
	mov edi, eax
	xor ebx, ebx
 	mov edx, 'PAMS'		; reverse of SMAP (0534D4150h)

.memory_map_cont:

	mov eax, 0xe820
	mov ecx, 20
	int 15h
	jc .memory_map_error
	add [memory_map_size], ecx
	add edi, ecx
	test ebx, ebx
	jnz .memory_map_cont
	jmp .memory_map_out

.memory_map_error:
	mov si, memory_map_error_msg
	call print_string

.memory_map_out:
	mov si, memory_map_msg
	call print_string

	xor eax, eax
	xor ebx, ebx
	mov eax, [memory_map_size]
	mov bl, 10
	call print_num

	pop si
	pop di
	pop edx
	pop ecx
	pop ebx
	pop eax
	ret

print_memory_map:
	push eax
	push ebx
	push ecx
	push edx
	xor edx, edx
	xor ecx, ecx
	xor ebx, ebx
	xor eax, eax

.print_memory_map_loop:
	mov eax, 0x4000
	add eax, ecx

	test edx, edx
	jnz .no_print_address
	;; print address
	call print_newline
	call print_hex

	mov si, double_dot
	call print_string

	mov si, hex_prepend
	call print_string

.no_print_address:	
	;; print value
	push eax
	mov eax, [eax]
	xor ebx, ebx
	call print_hex_raw
	pop eax
	
	mov ebx, 20
	xor edx, edx
	div ebx
	test edx, edx
	je .print_newline
	cmp edx, 4
	je .print_newline
	cmp edx, 8
	je .print_newline
	cmp edx, 12
	je .print_newline
	cmp edx, 16
	je .print_newline
	jmp .noprint_newline
.print_newline:
	add ecx, 4
	mov si, separator
	call print_string
.noprint_newline:
	pop eax
	
	cmp ecx, [memory_map_size]
	jne .print_memory_map_loop

	pop edx
	pop ecx
	pop ebx
	pop eax
	ret
	
boot_drive_msg		db "boot_drive: ", 0
boot_drive		db 0
sector_count_msg	db "sector_count: ", 0	
sector_count		db 0
memory_map_error_msg	db "Error getting memory map.", 0
memory_map_msg		db "Got memory map of size: ", 0
memory_map_size		dd 0
double_dot		db ": ", 0
hex_prepend		db "0x", 0
separator		db " - ", 0
	
memory_map_curr_base_addr	dd 0
memory_map_curr_length		dd 0

gdt_start:
	dq 0
gdt_code:			; index 1
	dw 0xFFFF
	dw 0x0000
	db 0x00
	db 10011010b
	db 11001111b
	db 0x00
gdt_data:			; index 2
	dw 0xFFFF
	dw 0x0000
	db 0x00
	db 10010010b
	db 11001111b
	db 0x00
gdt_end:

gdt_descriptor:
	dw gdt_end - gdt_start - 1
	dd gdt_start

[BITS 32]

idt_start:
times 256*8 db 0  ; 256 entries, zero-initialized
idt_end:

idt_descriptor:
    dw idt_end - idt_start - 1
    dd idt_start

; Load IDT

protected_mode_start:
	mov eax, 0x10       ; data selector
	mov ds, eax
	mov es, eax
	mov ss, eax
	mov fs, eax
	mov gs, eax
	mov sp, 0x8000

	lidt [idt_descriptor]

	call print_string_pm
hang:
	jmp hang


print_string_pm:
    pusha

    mov ebx, vga_buffer     ; destination pointer
    mov esi, message        ; pointer to string

.print_loop:
    lodsb                   ; AL = [ESI], ESI++
    or al, al
    jz .done                ; end if 0-terminator

    mov [ebx], al           ; write character
    mov byte [ebx+1], 0x07  ; attribute
    add ebx, 2              ; next cell
    jmp .print_loop

.done:
    popa
    ret

vga_buffer equ 0xB8000

message db "Hello Protected Mode!", 0	
msg db "Hello from boot stage 1.5...", 0
