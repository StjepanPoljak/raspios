#include <mmu/mmu.h>
#include <mem.h>
#include <log.h>

reg_t gpio = 0x40000000;

int main(int argc, const char* argv[]) {

	reg_t* var1;
	reg_t* var2;
	reg_t* var3;
	reg_t* var4;

	println("Welcome to the wonderful world of C!");

	mem_init();

	mmu_init();

	var1 = (reg_t*)alloc_fast(17);
	var2 = (reg_t*)alloc_fast(12);
	var3 = (reg_t*)alloc_fast(7);

	free(var2);

	dump_mem_accounting();

	var2 = (reg_t*)alloc_slow(3, ALLOC_SLOW_UP);
	var4 = (reg_t*)alloc_slow(2, ALLOC_SLOW_UP);

	free(var1);

	dump_mem_accounting();

	free(var2);

	/* rpi enable timer IRQ */
	__asm volatile(
		"adr	x0, irq_vector;"
		"msr	vbar_el1, x0;"
		"ldr	x1, =0x40003000;"
		"ldr	w0, [x1, #0x04];"
		"ldr	w2, =2000000;"
		"add	w0, w0, w2;"
		"str	w0, [x1, #0x10];"
		"mov	w0, #0x2;"
		"ldr	x1, =0x4000B210;"
		"str	w0, [x1];"
		"ldr	w0, =0x2000000;"
		"ldr	x1, =0x4000B214;"
		"str	w0, [x1];"
		"msr	daifclr, #2;"
		::
	);

	log(ln, "Timer initialized.", LOG_INFO);

	while (1) { }

	return 0;
}
