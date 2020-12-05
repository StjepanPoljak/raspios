EARLY_STACK_SIZE = 0x1000;
VA_START = (1 << VA_BITS) * ((1 << (64 - VA_BITS)) - 1);

SECTIONS
{
	. = VA_START;
	.text.boot : { *(.text.boot) }
	.data : { *(.data, .rodata) }
	. += EARLY_STACK_SIZE;
	. = ALIGN(16);
	_ld_early_stack = .;

	_ld_bss_start = .;
	.bss : { *(.bss) }
	_ld_bss_end = .;

	. = ALIGN(4096);
	_ld_tt_l1_base = .;
	. += 4096;
	_ld_tt_l1_base_end = .;

	_ld_heap = .;
}
