#ifndef MEM_H
#define MEM_H

#include "bridge.h"

#define mem_size_t uint16_t

/* don't use addr_t too much, this is
 * for raw pointers, so we override the
 * pointer offsets that C compiler
 * generates; also it's arch-specific */
#define addr_t uint64_t

#define align_t uint16_t

#define raw_ptr(ptr) (addr_t)(ptr)

/* Print variables from linker script. */
#define printld(var) print64((reg_t)(&var))

/* Print pointer. */
#define printptr(ptr) print64(raw_ptr(ptr))

reg_t _ld_heap;

struct memheader_t {
	struct memheader_t* prev;
	struct memheader_t* next;
	mem_size_t block_size;
	uint8_t active;
};

enum alloc_slow_mode {
	ALLOC_SLOW_UP,
	ALLOC_SLOW_DOWN
};

typedef struct memheader_t memheader_t;

/* Initializes the memory:
 *  - zeroes-out .bss section
 *  - sets up heap head and tail
 *  - resets accounting */
void mem_init(void);

/* Pads memory with regard to align_t. */
mem_size_t pad(mem_size_t, align_t);

/* Dumps memory accounting. */
void dump_mem_accounting(void);

/* Dumps memory */
void dump_mem(void*, unsigned int, unsigned int);

void* alloc_fast_align(mem_size_t, addr_t);

/* Fast allocation appends a memory
 * block to heap tail. */
void* alloc_fast(mem_size_t);

/* Slow allocation searches for holes
 * from heap head to heap tail and
 * inserts memory block at first free
 * spot (later we should consider
 * some different algorithms, like
 * to insert a memory block in the
 * place that it fits most tightly). */
void* alloc_slow(mem_size_t, enum alloc_slow_mode);

/* Free dynamically allocated pointer. */
void free(void*);

#endif
