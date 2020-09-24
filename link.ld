SECTIONS
{
	. = BOOT_ADDR;
	.text.boot : { *(.text.boot) }
	.data : { *(.data) }
	. += STACK_SIZE;
	. = ALIGN(16);
	stack = .;
}
