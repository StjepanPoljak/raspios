#include "mem.h"

#include "log.h"

static memheader_t* memlast;
static memheader_t* memfirst;

static mem_size_t alloc_size;
static mem_size_t hole_size;

static mem_size_t alloc_count;
static mem_size_t hole_count;

#define DEFAULT_ALIGN 8
#define align_default(size) align(size, DEFAULT_ALIGN)

#ifdef CONFIG_MEM_TRACE
#define _mem_trace(suffix, value) print ## suffix(value)
#define mem_trace(suffix, value, level) log(suffix, value, level)
#else
#define _mem_trace(suffix, value) ;;
#define mem_trace(suffix, value, level) ;;
#endif

reg_t* heap;

/* pad and align functions */

#define align(addr, algn) \
	((algn != 1) && (addr % (addr_t)algn) \
	? ((addr & ~((addr_t)(algn - 1))) + algn) \
	: addr)

static mem_size_t memheader_size(void) {

	static mem_size_t cache = 0;

	if (!cache) {
		cache = align_default((mem_size_t)(sizeof(memheader_t)));
		mem_trace(, "Allocated memheader_size cache@", LOG_INFO);
		_mem_trace(ptr, raw_ptr(&cache));
		_mem_trace(ln, "");
		_mem_trace(, "   -> cache=");
		_mem_trace(64, (uint64_t)cache);
		_mem_trace(ln, "");
	}
	return cache;
}

/* unit tests */

#ifdef CONFIG_MEM_TESTS
static void memheader_pad_test(void) {

	log_test(ln, "Testing memheader_t padding...");

	if ((memheader_size() % DEFAULT_ALIGN) != 0)
		log_fail(ln, "Non-alignment detected in memheader_t.");

	log_test(ln, "Alignment of memheader_t OK.");

	return;
}

static void pad_tests(void) {

	uint16_t vec[10] = { 2, 8, 11, 15, 24, 27, 28, 29, 101, 1000 };
	uint16_t res[10] = { 8, 8, 16, 16, 24, 32, 32, 32, 104, 1000 };
	uint8_t i;

	log_test(ln, "Starting align() unit test.");

	for (i = 0; i < 10; i++)
		if (align(vec[i], 8) != res[i]) {
			log_fail(ln, "Inconsistency detected in align().");
			_mem_trace(, " -> invalid alignment at i=");
			_mem_trace(64, i);
			_mem_trace(ln, "");

			return;
		}

	log_test(ln, "Unit test for align() passed.");

	return;
}

static void static_variable_integrity_test(void) {

	if (memlast || alloc_size || hole_size || alloc_count || hole_count)
		log_fail(ln, "Static variable integrity questionable.");
	else
		log_test(ln, "Static variable integrity test passed.");

	return;
}
#endif

/* debug help */

void dump_mem(void* start, unsigned int count, unsigned int cols) {
	uint64_t* i;
	uint64_t* end;
	uint8_t remainder;
	unsigned int col;
/*
	log_info(, "Called dump_mem(");
	_log(ptr, start);
	_log(, ", ");
	_log(64, count);
	_log(, ", ");
	_log(64, cols);
	_log(ln, ")");
*/
	remainder = count % cols;

	if (remainder) {
		end = start + (count - remainder) * sizeof(start);
	}
	else {
		end = start + count * sizeof(start);
	}

	for (i = (uint64_t*)start; i < end; i += cols) {
		print64_raw(raw_ptr(i));
		print(": ");

		for (col = 0; col < cols; col++) {
			print64_raw(*(i + col));

			if (col < cols - 1)
				print(" ");
		}
		newline();
	}

	if (remainder) {
		print64_raw(raw_ptr(i));
		print(": ");

		for (i = end; i < end + remainder; i += 1) {
			print64_raw(*i);
			if (i < end + remainder)
				print(" ");
		}
		newline();
	}
}

void dump_mem_accounting(void) {

	log_info(ln, "Dumping memory accounting data:");
	_log(, "   -> alloc_count = ");
	_log(64, alloc_count);
	_log(ln, "");
	_log(, "   -> alloc_size = ");
	_log(64, alloc_size);
	_log(ln, "");
	_log(, "   -> hole_count = ");
	_log(64, hole_count);
	_log(ln, "");
	_log(, "   -> hole_size = ");
	_log(64, hole_size);
	_log(ln, "");

	return;
}

/* init functions */

static void reset_heap(void) {

	memfirst = (memheader_t *)heap;
	memlast = 0;

	return;
}

static uint8_t heap_empty(void) {

	return memlast == 0;
}

void mem_init(void) {

	heap = (reg_t*)(&_ld_heap);

	reset_heap();

	mem_trace(, "Initializing memory (heap@", LOG_INFO);
	_mem_trace(ptr, memfirst);
	_mem_trace(ln, ").");


#ifdef CONFIG_MEM_TESTS
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

/* fast allocation */

static mem_size_t total_block_size(memheader_t* mh) {

	return mh->block_size + memheader_size();
}

void* alloc_fast(mem_size_t bsize) {

	return alloc_fast_align(bsize, 0x1);
}

memheader_t* align_data_after(memheader_t* last, addr_t align) {
	memheader_t* align_test;

	align_test = (memheader_t*)align(raw_ptr(last), align);
	while (raw_ptr(align_test) - memheader_size()
		< raw_ptr(last) + total_block_size(last))

		align_test = (memheader_t*)(raw_ptr(align_test) + align);

	return (memheader_t*)(raw_ptr(align_test) - memheader_size());
}

void* alloc_fast_align(mem_size_t bsize, addr_t align) {

	memheader_t* new_block;
	void* ret;

	mem_trace(, "Requested block of ", LOG_INFO);
	_mem_trace(64, bsize);
	_mem_trace(ln, " bytes (fast).");

	if (memlast != 0) {
		_mem_trace(, "   -> memlast@");
		_mem_trace(ptr, memlast);
		_mem_trace(ln, "");

		if (align != 0x1)
			new_block = align_data_after(memlast, align);
		else
			new_block = (memheader_t*)(raw_ptr(memlast)
				    + total_block_size(memlast));
		memlast->next = new_block;
	}
	else {
		_mem_trace(ln, "   -> first allocation (fast) in heap");
		if (align != 0x1)
			memfirst = align_data_after(memfirst, align);
		new_block = memfirst;
	}

	*new_block = (memheader_t){
		.next = 0,
		.prev = memlast != 0 ? memlast : 0,
		.block_size = align(bsize, DEFAULT_ALIGN),
		.active = 1
	};

	memlast = new_block;
	ret = (void*)(raw_ptr(new_block) + memheader_size());

	account_fast(total_block_size(new_block));

	_mem_trace(, "   -> alloc@");
	_mem_trace(ptr, new_block);
	_mem_trace(, " (data@");
	_mem_trace(ptr, ret);
	_mem_trace(ln, ").");
	_mem_trace(, "   -> size=");
	_mem_trace(64, (uint64_t)total_block_size(new_block));
	_mem_trace(ln, "");

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
		.block_size = align_default(ms),
		.active = 1
	};

	account_slow(total_block_size(new_block));

	ret = (void*)(raw_ptr(new_block) + (addr_t)new_block->block_size);

	_mem_trace(, "   -> alloc@");
	_mem_trace(ptr, new_block);
	_mem_trace(, " (data@");
	_mem_trace(ptr, ret);
	_mem_trace(ln, ").");

	return ret;
}

static memheader_t* get_memheader_of(void* ptr) {

	memheader_t* res = (memheader_t*)(raw_ptr(ptr) - memheader_size());

	_mem_trace(, "   -> memheader_of(");
	_mem_trace(ptr, raw_ptr(ptr));
	_mem_trace(, ")@");
	_mem_trace(ptr, raw_ptr(res));
	_mem_trace(ln, "");
	_mem_trace(, "   -> memheader_size=");
	_mem_trace(64, memheader_size());
	_mem_trace(ln, "");

	return res;
	
}

void* alloc_slow(mem_size_t size, enum alloc_slow_mode mode) {

	void* ret;
	memheader_t* curr;
	mem_size_t tbsize;
	mem_size_t memdiff;
	mem_size_t tbsize_curr;

	mem_trace(, "Requested block of ", LOG_INFO);
	_mem_trace(64, size);
	_mem_trace(ln, " bytes (slow).");

	tbsize = memheader_size() + align_default(size);
	ret = 0;

	if (mode == ALLOC_SLOW_UP) {

		if ((mem_size_t)(raw_ptr(memfirst) - (addr_t)(heap))
		    >= tbsize) {

			_mem_trace(, "   -> prepending (memfirst@");
			_mem_trace(ptr, memfirst);
			_mem_trace(, ", heap@");
			_mem_trace(ld, *heap);
			_mem_trace(ln, ").");

			ret = insert(size, (addr_t)(memfirst)
					 - (addr_t)(tbsize), 0, memfirst);
			memfirst = get_memheader_of(ret);

			_mem_trace(, "   -> inserted (memfirst@");
			_mem_trace(ptr, memfirst);
			_mem_trace(ln, ").");

			return ret;
		}

		curr = memfirst;

		while (curr->next) {

			tbsize_curr = total_block_size(curr);
			memdiff = (mem_size_t)(raw_ptr(curr->next)
					     - raw_ptr(curr));

			if ((memdiff - tbsize_curr) >= tbsize) {

				_mem_trace(, "   -> found hole of size = ");
				_mem_trace(64, memdiff);
				_mem_trace(ln, "");

				ret = insert(size, raw_ptr(curr)
					  + (addr_t)tbsize_curr,
					     curr, curr->next);

				return ret;
			}
			curr = curr->next;
		}
	}

	mem_trace(ln, "No suitable holes in heap, "
		      "resorting to alloc_fast().", LOG_INFO);

	return alloc_fast(size);
}

/* free function */

void free(void* ptr) {

	memheader_t* curr;

	if (heap_empty()) {
		log_error(, "Attempting to free from empty heap (addr@");
		_log(ptr, ptr);
		_log(ln, ").");

		return;
	}

	curr = get_memheader_of(ptr);

	if (!curr->active) {
		log_error(, "Double free (ptr@");
		_log(ptr, ptr);
		_log(ln, ").");

		return;
	}

	mem_trace(, "Freeing: ", LOG_INFO);
	_mem_trace(ptr, ptr);
	_mem_trace(, " (memheader@");
	_mem_trace(ptr, curr);
	_mem_trace(ln, ").");

	if (curr->next && curr->prev) {
		_mem_trace(ln, "   -> hole created");

		account_hole(total_block_size(curr));
		curr->next->prev = curr->prev;
		curr->prev->next = curr->next;
	}
	else if (curr->prev) {
		_mem_trace(ln, "   -> removed tail");

		account_free(total_block_size(curr));
		curr->prev->next = 0;
		memlast = curr->prev;
	}
	else if (curr->next) {
		_mem_trace(ln, "   -> removed head");

		account_hole(total_block_size(curr));
		curr->next->prev = 0;
		memfirst = curr->next;
	}
	else {
		_mem_trace(ln, "   -> heap empty");

		reset_heap();
		alloc_count = 0;
		alloc_size = 0;
		hole_count = 0;
		hole_size = 0;
	}

	curr->active = 0;

	return;
}
