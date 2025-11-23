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
	mov bl, 0x10
	call print_num
	call print_newline

	call memory_map
	
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
	mov di, ax
	xor ebx, ebx

.memory_map_cont:
	mov eax, 0xe820
	mov ecx, 20
 	mov edx, 'PAMS'		; reverse of SMAP (0534D4150h)
	int 15h
	jc .memory_map_error
	add [memory_map_size], ebx
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
	mov ax, [memory_map_size]
	mov bl, 10
	call print_num

	pop si
	pop di
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
