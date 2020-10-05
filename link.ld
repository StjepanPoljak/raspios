SECTIONS
{
	. = BOOT_ADDR;
	.text.boot : { *(.text.boot) }
	.data : { *(.data) }
	heap = .;
}
