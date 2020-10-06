#include "mem.h"

static memheader_t* memlast;
static memheader_t* memfirst;

static mem_size_t alloc_size;
static mem_size_t hole_size;

static mem_size_t alloc_count;
static mem_size_t hole_count;

#define MEM_TRACE
#define MEM_TESTS

#define DEFAULT_ALIGN 8
#define pad_default(size) pad(size, DEFAULT_ALIGN)

#ifdef MEM_TRACE
#define memlog(suffix, value) print ## suffix(value)
#else
#define memlog(suffix, value) ;;
#endif

/* pad and align functions */

mem_size_t pad(mem_size_t ms, align_t algn) {

	if ((((align_t)ms) % algn) == 0)
		return ms;
	else
		return ((((align_t)ms) / algn) + 1) * algn;
}

static mem_size_t memheader_size(void) {

	static mem_size_t cache = 0;

	if (!cache)
		cache = pad((mem_size_t)(sizeof(memheader_t)), DEFAULT_ALIGN);

	return cache;
}

/* unit tests */

#ifdef MEM_TESTS
static void memheader_pad_test(void) {

	memlog(ln, "(1) Testing memheader_t padment...");

	if ((memheader_size() % DEFAULT_ALIGN) != 0)
		memlog(ln, "(!) Non-padment detected in memheader_t.");

	memlog(ln, "(1) Alignment of memheader_t OK.");

	return;
}

static void pad_tests(void) {

	uint16_t vec[10] = { 2, 8, 11, 15, 24, 27, 28, 29, 101, 1000 };
	uint16_t res[10] = { 8, 8, 16, 16, 24, 32, 32, 32, 104, 1000 };
	uint8_t i;

	memlog(ln, "(2) Starting pad() unit test.");

	for (i = 0; i < 10; i++)
		if (pad(vec[i], 8) != res[i]) {
			memlog(ln, "(!) Inconsistency detected in pad().");
			memlog(, " -> invalid padment at i=");
			memlog(64, i);
			memlog(ln, "");

			return;
		}

	memlog(ln, "(2) Unit test for pad() passed.");

	return;
}

static void static_variable_integrity_test(void) {

	if (memlast || alloc_size || hole_size || alloc_count || hole_count)
		memlog(ln, "(!) Static variable integrity questionable.");
	else
		memlog(ln, "(3) Static variable integrity test passed.");

	return;
}
#endif

/* init functions */

static void reset_heap(void) {

	memfirst = (memheader_t*)(&heap);
	memlast = 0;

	return;
}

static uint8_t heap_empty(void) {

	return memlast == 0;
}

void mem_init(void) {

	__asm__(
		"adr x0, bss_start;"
		"adr x1, bss_end;"
		"bss_zero:"
		"	strb wzr, [x0];"
		"	add x0, x0, #1;"
		"	cmp x0, x1;"
		"	b.lt bss_zero;");

	reset_heap();

	memlog(, "Initializing memory (heap@");
	memlog(ptr, memfirst);
	memlog(ln, ").");


#ifdef MEM_TESTS
	memheader_pad_test();
	pad_tests();
	static_variable_integrity_test();
#endif

	return;
}


/* accounting functions */

static void account_free(mem_size_t ms) {

	alloc_count--;
	alloc_size -= ms;

	return;
}

static void account_hole(mem_size_t ms) {

	account_free(ms);
	hole_count++;
	hole_size += ms;

	return;
}

static void account_fast(mem_size_t ms) {

	alloc_count++;
	alloc_size += ms;

	return;
}

static void account_slow(mem_size_t ms) {

	account_fast(ms);
	hole_count--;
	hole_size -= ms;

	return;
}

void dump_mem_accounting(void) {

	memlog(ln, "(i) Dumping memory accounting data:");
	memlog(, "   -> alloc_count = ");
	memlog(64, alloc_count);
	memlog(ln, "");
	memlog(, "   -> alloc_size = ");
	memlog(64, alloc_size);
	memlog(ln, "");
	memlog(, "   -> hole_count = ");
	memlog(64, hole_count);
	memlog(ln, "");
	memlog(, "   -> hole_size = ");
	memlog(64, hole_size);
	memlog(ln, "");

	return;
}

/* fast allocation */

static mem_size_t total_block_size(memheader_t* mh) {

	return mh->block_size + memheader_size();
}

void* alloc_fast(mem_size_t bsize) {

	memheader_t* new_block;
	void* ret;

	memlog(, "(i) Requested block of ");
	memlog(64, bsize);
	memlog(ln, " bytes (fast).");

	if (memlast != 0) {
		memlog(, "   -> memlast@");
		memlog(ptr, memlast);
		memlog(ln, "");
		new_block = (memheader_t*)(raw_ptr(memlast)
			    + total_block_size(memlast));
		memlast->next = new_block;
	}
	else {
		memlog(ln, "   -> first allocation (fast) in heap");
		new_block = memfirst;
	}

	*new_block = (memheader_t){
		.next = 0,
		.prev = memlast != 0 ? memlast : 0,
		.block_size = pad(bsize, DEFAULT_ALIGN),
		.active = 1
	};

	memlast = new_block;
	ret = (void*)(raw_ptr(new_block) + memheader_size());

	account_fast(total_block_size(new_block));

	memlog(, "   -> alloc@");
	memlog(ptr, new_block);
	memlog(, " (data@");
	memlog(ptr, ret);
	memlog(ln, ").");
	memlog(, "   -> size=");
	memlog(64, (uint64_t)total_block_size(new_block));
	memlog(ln, "");

	return ret;
}

/* slow allocation */

static void* insert(mem_size_t ms, addr_t addr,
		    memheader_t* prev, memheader_t* next) {

	memheader_t* new_block;
	void* ret;

	new_block = (memheader_t*)addr;

	if (next)
		next->prev = new_block;
	
	if (prev)
		prev->next = new_block;

	*new_block = (memheader_t){
		.next = next,
		.prev = prev,
		.block_size = pad(ms, DEFAULT_ALIGN),
		.active = 1
	};

	account_slow(total_block_size(new_block));

	ret = (void*)(raw_ptr(new_block) + (addr_t)new_block->block_size);

	memlog(, "   -> alloc@");
	memlog(ptr, new_block);
	memlog(, " (data@");
	memlog(ptr, ret);
	memlog(ln, ").");

	return ret;
}

static memheader_t* get_memheader_of(void* ptr) {

	return (memheader_t*)(raw_ptr(ptr) - memheader_size());
}

void* alloc_slow(mem_size_t size, enum alloc_slow_mode mode) {

	void* ret;
	memheader_t* curr;
	mem_size_t tbsize;
	mem_size_t memdiff;
	mem_size_t tbsize_curr;

	memlog(, "(i) Requested block of ");
	memlog(64, size);
	memlog(ln, " bytes (slow).");

	tbsize = memheader_size() + pad(size, DEFAULT_ALIGN);
	ret = 0;

	if (mode == ALLOC_SLOW_UP) {

		if ((mem_size_t)(raw_ptr(memfirst) - (addr_t)(&heap))
		    >= tbsize) {

			memlog(, "   -> prepending (memfirst@");
			memlog(ptr, memfirst);
			memlog(, ", heap@");
			memlog(ld, heap);
			memlog(ln, ").");

			ret = insert(size, (addr_t)(&heap), 0, memfirst);
			memfirst = get_memheader_of(ret);

			return ret;
		}

		curr = memfirst;

		while (curr->next) {

			tbsize_curr = total_block_size(curr);
			memdiff = (mem_size_t)(raw_ptr(curr->next) - raw_ptr(curr));

			if ((memdiff - tbsize_curr) >= tbsize) {

				memlog(, "   -> found hole of size = ");
				memlog(64, memdiff);
				memlog(ln, "");

				ret = insert(size,
					     raw_ptr(curr) + (addr_t)tbsize_curr,
					     curr, curr->next);

				return ret;
			}
			curr = curr->next;
		}
	}

	memlog(ln, "(!) No suitable holes in heap, resorting to alloc_fast().");

	return alloc_fast(size);
}

/* free function */

void free(void* ptr) {

	memheader_t* curr;

	if (heap_empty()) {
		memlog(, "(!) Attempting to free from empty heap (addr@");
		memlog(ptr, ptr);
		memlog(ln, ").");

		return;
	}

	curr = get_memheader_of(ptr);

	if (!curr->active) {
		memlog(, "(!) Double free (ptr@");
		memlog(ptr, ptr);
		memlog(ln, ").");

		return;
	}

	memlog(, "(i) Freeing: ");
	memlog(ptr, ptr);
	memlog(, " (memheader@");
	memlog(ptr, curr);
	memlog(ln, ").");

	if (curr->next && curr->prev) {
		memlog(ln, "   -> hole created");

		account_hole(total_block_size(curr));
		curr->next->prev = curr->prev;
		curr->prev->next = curr->next;
	}
	else if (curr->prev) {
		memlog(ln, "   -> removed tail");

		account_free(total_block_size(curr));
		curr->prev->next = 0;
		memlast = curr->prev;
	}
	else if (curr->next) {
		memlog(ln, "   -> removed head");

		account_hole(total_block_size(curr));
		curr->next->prev = 0;
		memfirst = curr->next;
	}
	else {
		memlog(ln, "   -> heap empty");

		reset_heap();
		alloc_count = 0;
		alloc_size = 0;
		hole_count = 0;
		hole_size = 0;
	}

	curr->active = 0;

	return;
}
