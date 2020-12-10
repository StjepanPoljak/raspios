#include <ptable.h>

#include <mem.h>

extern void memzero(addr_t, addr_t);

ptable_t gpt;

void dump_table(ptable_t *table) {

	dump_mem((void*)table->raw_table, table->num_entries, 8);

	return;
}

static raw_ptable_t create_raw_table(unsigned int num_entries) {
	raw_ptable_t raw_table;
	unsigned int table_size;

	table_size = num_entries * sizeof(pentry_t);
	raw_table = (raw_ptable_t)alloc_fast_align(table_size, 4096);
	memzero(raw_ptr(raw_table), raw_ptr(raw_table) + table_size);

	return raw_table;
}

/* initialize page table */
void ptable_init(ptable_t* table, unsigned int entry_span,
		 unsigned int num_entries) {

	table->raw_table = create_raw_table(num_entries);
	table->last = 0;
	table->entry_span = entry_span;
	table->num_entries = num_entries;
}

/* get last free entry in page table */
pentry_t* ptable_get_free_entry(ptable_t* table) {

	if (table->last >= table->raw_table + (table->num_entries - 1))
		return 0;

	table->last = !table->last ? table->raw_table : table->last + 1;

	return table->last;
}

/* create table from entry in parent table and inherit num_entries */
void ptable_init_from(ptable_t* parent, ptable_t* table) {

	ptable_init(table, parent->entry_span / parent->num_entries,
		    parent->num_entries);

	return;
}

/* GPT-related */

ptable_t* ptable_get_gpt(void) {

	return &gpt;
}

void ptable_gpt_init(unsigned int entry_span, unsigned int num_entries) {

	ptable_init(&gpt, entry_span, num_entries);
}
