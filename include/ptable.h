#ifndef PTABLE_H
#define PTABLE_H

#include <stdint.h>

#define pentry_t uint64_t
#define raw_ptable_t pentry_t*

#define for_each_pte_in(table, entry) \
	while ((entry = ptable_get_free_entry(table)))

/*	for (entry = (table)->raw_table; \
	     entry < ((table)->raw_table + (table)->num_entries); \
	     entry++) */

typedef struct ptable_t {
	raw_ptable_t raw_table;
	pentry_t* last;
	unsigned int entry_span;
	unsigned int num_entries;
} ptable_t;

void dump_table(ptable_t *table);

void ptable_init(ptable_t* table, unsigned int entry_span,
		 unsigned int num_entries);
pentry_t* ptable_get_free_entry(ptable_t* table);
void ptable_init_from(ptable_t* parent, ptable_t* table);

ptable_t* ptable_get_gpt(void);
void ptable_gpt_init(unsigned int entry_span, unsigned int num_entries);

#endif
