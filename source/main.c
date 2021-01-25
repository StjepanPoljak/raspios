#include <mmu/mmu.h>
#include <mem.h>
#include <log.h>

#include <mailbox.h>

addr_t gpio = 0x40000000;

#define TEMPERATURE_TAG 0x30006
#define MAX_CLOCK_RATE_TAG 0x30004
#define PROPERTY_CHANNEL 0x8

DEFINE_MAILBOX_BUFFER(msg, 2);

int main(int argc, const char* argv[]) {

	addr_t phaddr;
	reg_t* var1;
	reg_t* var2;
	reg_t* var3;
	reg_t* var4;
	uint32_t tid[1] = { 0 };
	uint32_t val;

	println("Welcome to the wonderful world of C!");

	mem_init();

	mbox_init(0x3f000000 | DEFAULT_MAILBOX_BASE);
	create_msg(2, msg, TEMPERATURE_TAG, 1, tid);
	mbox_comm(msg, PROPERTY_CHANNEL);
	mbox_get(msg, 1, &val);

	log(,"Got temperature: ", LOG_INFO);
	_log(64, (uint64_t)val);
	_log(ln, "");

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
