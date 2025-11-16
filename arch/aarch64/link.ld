VA_START = (1 << CONFIG_VA_BITS) * ((1 << (64 - CONFIG_VA_BITS)) - 1);

ENTRY (_start)

SECTIONS
{
	. = CONFIG_LOW_LINK_BASE;

	.text.boot : {
		   _start = .;
		   KEEP(*(.text.boot._start))
		   KEEP(*(.text.boot*))
	}
	.data.boot : { KEEP(*(.data.boot*)) }
	.rodata.boot : { KEEP(*(.rodata.boot*)) }
	. = ALIGN(16);
	. += CONFIG_EARLY_STACK_SIZE;
	. = ALIGN(16);
	_ld_stack = .;

	_ld_bss_boot_start = .;
	.bss.boot : { *(.bss.boot*) }
	_ld_bss_boot_end = .;

	. = ALIGN(0x1000);
	.ptables.boot : {
		KEEP(*(.ptables.boot*))
	}


	. = ALIGN(0x1000);

	_kernel_phys_start = .;
	. = VA_START;
	_kernel_virt_start = .;

	.text : AT(_kernel_phys_start) {
	      KEEP(*(.text*)) }
	.rodata : { KEEP(*(.rodata*)) }
	.data : { KEEP(*(.data*)) }
	.bss : { KEEP(*(.bss*)) }
	. = ALIGN(0x1000);
	.dtb :{ KEEP(*(.dtb)) }

	. += CONFIG_STACK_SIZE;
	. = ALIGN(16);
	_ld_stack_high = .;
/*	_ld_heap = .; */

}
