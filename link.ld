SECTIONS
{
	. = BOOT_ADDR;
	.text.boot : { *(.text.boot) }
	.data : { *(.data, .rodata) }
	. = ALIGN(16);
	bss_start = .;
	.bss : { *(.bss) }
	. = ALIGN(16);
	bss_end = .;
	. = ALIGN(4096);
	tt_l1_base = .;
	. += 4096;
	tt_l1_base_end = .;
	. = ALIGN(16);
	heap = .;
}
