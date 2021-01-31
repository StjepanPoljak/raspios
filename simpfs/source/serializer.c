#include "serializer.h"

#include <stdlib.h>
#include <errno.h>
#include <string.h>

#include "common.h"
#include "simpnode.h"

static const char* basename(const char* path) {
	unsigned int i;

	for (i = strlen(path); i >= 0; i--)
		if (path[i] == '/')
			return &(path[i + 1]);

	return path;
}

void init_data(data_t* data) {

	data->data = NULL;
	data->size = 0;
	data->last = -1;

	return;
}

void deinit_data(data_t* data) {

	if (data->data)
		free(data->data);

	return;
}

static int add_byte(data_t* data, uint8_t byte) {

	data->last++;

	if (data->size == 0) {
		data->size = 2;
		data->data = malloc(sizeof(*data) * data->size);
	}
	else if (data->size == data->last + 1) {
		data->size *= 2;
		data->data = realloc(data->data, sizeof(*data) * data->size);
	}

	if (!data->data) {
		logs_err("Could not allocate memory.");
		return -ENOMEM;
	}

	data->data[data->last] = byte;

	return 0;
}

#define GETB(VAL, POS, TYPE) \
	(((VAL) & ((TYPE)0xFF << (8 * (POS)))) >> (8 * (POS)))

static int u32_to_u8(uint32_t val, int(*op)(uint8_t, int, void*), void* data) {
	int i, ret;

	if (!op)
		return -EINVAL;

	for (i = 0; i < sizeof(val); i++)
		if ((ret = op(GETB(val, sizeof(val) - i - 1, uint32_t),
			      i, data)) < 0)
			return ret;

	return 0;
}

int add_data(data_t* data, const uint8_t* new_data, int bytes) {
	int i, ret;

	for(i = 0; i < bytes; i++)
		if ((ret = add_byte(data, new_data[i])) < 0)
			return ret;

	return 0;
}

static uint8_t* get_last_ptr(data_t* data) {

	return &(data->data[data->last]);
}

/*
uint8_t type
uint8_t ... name \0
if (type == FILE)
uint32_t data_size
uint8_t data
elif (type == DIR)
uint32_t child_num
uint32_t child1_loc
uint32_t child2_loc
...
uint32_t child3_loc
endif

*/

typedef struct {
	//uint8_t* ptr;
	uint32_t adr;
	struct simpnode_t* child;
} child_node_t;

int data_op(uint8_t byte, int i, void* udata) {
	data_t* data;
	int ret;

	(void)i;

	data = (data_t*)udata;

	if ((ret = add_byte(data, byte)) < 0)
		return ret;

	return 0;
}

static child_node_t* children = NULL;
static size_t child_num = 0;

#define GETC(CURR, I) \
	(CURR)->simpnode.dir.children[(I)]

void callback(struct simpnode_t* curr, void* udata) {
	data_t* data;
	int i;
	int prev_child_num;
	uint32_t data_last;

	data = (data_t*)udata;
	prev_child_num = child_num;

	for (i = 0; i < prev_child_num; i++) {
		if (children[i].child == curr) {
			printf("%.4x\n", data->last);
			data_last = data->last + 1;
			memcpy(&(data->data[children[i].adr]), &data_last, sizeof(data_last));
			break;
		}
	}

	add_byte(data, curr->simpnode_type);

	for (i = 0; i < strlen(basename(curr->name)); i++)
		add_byte(data, basename(curr->name)[i]);
	add_byte(data, 0);

	switch (curr->simpnode_type) {
	case SIMPNODE_DIRECTORY:
		u32_to_u8(curr->simpnode.dir.child_lst + 1, data_op, data);
		child_num = child_num + curr->simpnode.dir.child_lst + 1;

		if (!children)
			children = malloc(sizeof(*children) *
					  child_num);
		else
			children = realloc(children, sizeof(*children) *
					   child_num);

		for (i = prev_child_num; i < child_num; i++) {
			children[i].adr = data->last;
			children[i].child = GETC(curr, i - prev_child_num);
			u32_to_u8(0, data_op, data);

		}
		break;

	case SIMPNODE_FILE:
		u32_to_u8(curr->simpnode.file.data_size, data_op, data);
		add_data(data, curr->simpnode.file.data, curr->simpnode.file.data_size);
		break;
	}

	return;
}

data_t* serialize(const char* curr_dir) {
	struct simpnode_t* root;
	data_t* data;

	data = malloc(sizeof(*data));

	init_data(data);

	root = create_tree(curr_dir);
	if (!root)
		return NULL;

	deinit_simpnode(root, callback, data);

	return data;
}
