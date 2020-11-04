#include "mmu.h"
#include "mem.h"

int main(int argc, const char* argv[]) {

	reg_t* var1;
	reg_t* var2;
	reg_t* var3;
	reg_t* var4;

	mmu_init();

	println("Welcome to the wonderful world of C!");

	println("");

	mem_init();

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

	while (1) { }

	return 0;
}
