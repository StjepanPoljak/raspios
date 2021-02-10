VA_START = (1 << CONFIG_VA_BITS) * ((1 << (64 - CONFIG_VA_BITS)) - 1);

SECTIONS
{
	. = 0x1000000;
	.text.boot : { *(.text.boot) }
	. = ALIGN(16);
	.data : { *(.data, .rodata) }
	. += CONFIG_STACK_SIZE;
	. = ALIGN(16);
	_ld_stack = .;

	_ld_bss_start = .;
	.bss : { *(.bss) }
	_ld_bss_end = .;

	_ld_heap = .;
}
