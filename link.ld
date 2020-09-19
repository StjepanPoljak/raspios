STACK_SIZE = 0x1000;

SECTIONS
{
	. = 0x01000000;
	.text.boot : { *(.text.boot) }
	.data : { *(.data) }
	. += STACK_SIZE;
	stack = .;
}
