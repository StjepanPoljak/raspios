#include <mmu.h>
#include <log.h>
#include <util.h>

#include <mailbox.h>

#define TEMPERATURE_TAG 0x30006
#define MAX_CLOCK_RATE_TAG 0x30004
#define PROPERTY_CHANNEL 0x8

DEFINE_MAILBOX_BUFFER(msg, 2);

uint64_t _kernel_load_addr_high;

void prstr(const char *str) {

	/* x3 stores link base for boot PA
	 * x4 stores kernel load PA moved to high VA */
	__asm volatile(
		"mov x3, %0;"
		"adr x5, _kernel_load_addr_high;"
		"ldr x4, [x5];"
		
	        "ldr x2, =print_string;"
		"sub x2, x2, x3;"
		"add x2, x2, x4;"

		"mov x0, %1;"
		"ldr x1, =uart_write_char;"
		"sub x1, x1, x3;"
		"add x1, x1, x4;"

		"br x2;" : : "r"(CONFIG_LOW_LINK_BASE), "r"(str));
}

int high_va_entry() {
	prstr("Hello world, high VA!\n");
	/* rpi enable timer IRQ */
    	__asm volatile(
		"adr	x0, irq_vector;"
		"msr	vbar_el1, x0;"
		"ldr	x1, =0x3f003000;"
		"ldr	w0, [x1, #0x04];"
		"ldr	w2, =2000000;"
		"add	w0, w0, w2;"
		"str	w0, [x1, #0x10];"
		"mov	w0, #0x2;"
		"ldr	x1, =0x3f00B210;"
		"str	w0, [x1];"
		"ldr	w0, =0x2000000;"
		"ldr	x1, =0x3f00B214;"
		"str	w0, [x1];"
		"msr	daifclr, #2;"
		::
	);

	prstr("Initialized IRQ\n");
	
	while (1) { }
}

__boot void high_va_jump(void) {
	asm volatile(
		"ldr x0, =_ld_stack_high;"
		"mov sp, x0;"

		"adr x0, _kernel_load_addr;"
		"ldr x1, [x0];"
		"ldr x0, =_kernel_load_addr_high;"
		"str x1, [x0];"

		"ldr x0, =high_va_entry;"
		"br      x0");
}

__boot int main(int argc, const char* argv[]) {

	mmu_init();
	high_va_jump();

	return 0;
}
