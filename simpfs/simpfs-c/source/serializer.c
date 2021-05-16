#include "serializer.h"

#include <stdlib.h>
#include <errno.h>
#include <string.h>

#include "common.h"
#include "simpnode.h"

typedef struct {
	uint32_t adr;
	struct simpnode_t* child;
} child_node_t;

static child_node_t* children = NULL;
static size_t child_num = 0;

#define GETC(CURR, I) \
	(CURR)->simpnode.dir.children[(I)]

#define GETB(VAL, POS, TYPE) \
	(((VAL) & ((TYPE)0xFF << (8 * (POS)))) >> (8 * (POS)))

void init_data(data_t* data) {

	data->data = NULL;
	data->size = 0;
	data->last = -1;

	return;
}

void deinit_data(data_t* data) {

	if (data->data)
		free(data->data);

	free(data);

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

int data_op(uint8_t byte, int i, void* udata) {
	data_t* data;
	int ret;

	(void)i;

	data = (data_t*)udata;

	if ((ret = add_byte(data, byte)) < 0)
		return ret;

	return 0;
}

static int export_binary(data_t* data, const char* outfile) {
	int len;
	FILE *f;

	f = fopen(outfile, "w");
	if (!f) {
		logs_err("Could not open %s for output.", outfile);

		return -EINVAL;
	}

	len = data->last + 1;

	if (fwrite(data->data, 1, len, f) != len) {
		logs_err("Error writing data to %s.", outfile);

		return -ENOMEM;
	}

	fclose(f);

	return 0;
}

static void callback(struct simpnode_t* curr, void* udata) {
	data_t* data;
	int i;
	int prev_child_num;
	uint32_t data_last;

	data = (data_t*)udata;
	prev_child_num = child_num;

	for (i = 0; i < child_num; i++) {
		if (children[i].child == curr) {
			data_last = data->last + 1;
			memcpy(&(data->data[children[i].adr]),
			       &data_last, sizeof(data_last));
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

		children = !children
			 ? malloc(sizeof(*children) * child_num)
			 : realloc(children, sizeof(*children) * child_num);

		for (i = prev_child_num; i < child_num; i++) {
			children[i].adr = data->last + 4;
			children[i].child = GETC(curr, i - prev_child_num);
			u32_to_u8(0, data_op, data);
		}

		break;

	case SIMPNODE_FILE:
		u32_to_u8(curr->simpnode.file.data_size, data_op, data);

		if (curr->simpnode.file.data_size > 0)
			add_data(data, curr->simpnode.file.data,
				 curr->simpnode.file.data_size);

		break;
	}

	return;
}

data_t* serialize(const char* curr_dir, const char* outfile) {
	struct simpnode_t* root;
	data_t* data;

	data = malloc(sizeof(*data));

	init_data(data);

	root = create_tree(curr_dir);
	if (!root)
		return NULL;

#ifdef SIMP_DEBUG
	print_tree(root);
#endif
	deinit_simpnode(root, callback, data);

	export_binary(data, outfile);

	if (children)
		free(children);

	return data;
}

