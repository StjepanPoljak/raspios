bits 16
org 7c00h

start:
	cli
	
	xor ax, ax
	mov ds, ax
	mov es, ax
	mov ss, ax
	mov fs, ax
	mov gs, ax

	mov [boot_drive], dl

	mov sp, 0x7c00
	sti

	xor ax, ax
	mov es, ax		; set ES=0
	mov al, [sector_count]
	mov ah, 0x2		; read sectors from drive
	mov ch, 0 		; starting cylinder
	mov cl, 2		; 1 is MBR (this), so start from 2
	mov dh, 0		; starting head
	mov dl, [boot_drive]
	mov bx, 0x8000
	int 13h

	xor ax, ax
	xor bx, bx

	mov al, [boot_drive]
	mov ah, [sector_count]
	jmp 0x8000
hang:
	jmp hang

boot_drive		db 0
sector_count		db SECTOR_COUNT

times 510-($-$$)db 0
dw 0xAA55

