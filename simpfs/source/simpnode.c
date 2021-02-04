#include "simpnode.h"

#include <stdbool.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <sys/types.h>	/* DIR */
#include <dirent.h>	/* opendir() */
#include <unistd.h>	/* getcwd() */

#include "common.h"
#include "serializer.h"

#define DIRPR(NODE, PROP) \
	(NODE)->simpnode.dir.PROP

typedef struct {
	struct simpnode_t** stack;
	int last;
	int size;
} stack_t;

static void init_sndir(struct simpnode_t* node, const char* name) {

	node->name = name ? strdup(name) : NULL;
	node->level = 0;
	node->simpnode_type = SIMPNODE_DIRECTORY;
	node->simpnode.dir = (simpnode_dir_t) {
		.child_num = 0,
		.child_lst = -1,
		.children = NULL,
		.raw_children = NULL
	};

	return;
}

static int init_snfile(struct simpnode_t* node, const char* name) {
	uint8_t* data;
	unsigned int data_size;

	node->name = name ? strdup(name) : NULL;
	if (!node->name && name) {
		logs_err("Error executing strdup().");
		return -ENOMEM;
	}

	data = name ? load_file(name, &data_size) : NULL;

	node->level = 0;
	node->simpnode_type = SIMPNODE_FILE;
	node->simpnode.file = (simpnode_file_t) {
		.data_size = data_size,
		.data = data
	};

	return 0;
}

static int add_child(struct simpnode_t* parent, struct simpnode_t* child) {

	if (parent->simpnode_type != SIMPNODE_DIRECTORY) {
		logs_err("Adding child to non-directory node.");
		return -EINVAL;
	}

	DIRPR(parent, child_lst)++;

	if (DIRPR(parent, child_num) == 0) {
		DIRPR(parent, child_num) = 2;
		DIRPR(parent, children) = malloc(DIRPR(parent, child_num) *
					sizeof(*(DIRPR(parent, children))));
		if (!DIRPR(parent, children)) {
			logs_err("Could not allocate memory.");
			return -ENOMEM;
		}
	}
	else if (DIRPR(parent, child_num) == DIRPR(parent, child_lst) + 1) {
		DIRPR(parent, child_num) *= 2;
		DIRPR(parent, children) = realloc(DIRPR(parent, children),
						  DIRPR(parent, child_num) *
					 sizeof(*(DIRPR(parent, children))));
		if (!DIRPR(parent, children)) {
			logs_err("Could not allocate memory.");
			return -ENOMEM;
		}
	}

	DIRPR(parent, children)[DIRPR(parent, child_lst)] = child;

	return 0;
}

static void init_stack(stack_t* stack) {
	stack->stack = NULL;
	stack->last = -1;
	stack->size = 0;

	return;
}

static void deinit_stack(stack_t* stack) {

	if (stack->stack)
		free(stack->stack);

	return;
}

static int push_simpnode(stack_t* stack, struct simpnode_t* node) {

	stack->last++;

	if (stack->size == 0) {
		stack->size = 2;
		stack->stack = malloc(sizeof(*(stack->stack)) * stack->size);
		if (!stack->stack)
			return -ENOMEM;
	}
	else if (stack->size == stack->last + 1) {
		stack->size = stack->size * 2;
		stack->stack = realloc(stack->stack, sizeof(*(stack->stack))
				     * stack->size);
		if (!stack->stack)
			return -ENOMEM;

	}

	stack->stack[stack->last] = node;

	return 0;
}

static struct simpnode_t* pop_simpnode(stack_t* stack) {

	if (stack->last == -1)
		return NULL;

	return stack->stack[stack->last--];
}

static void deinit_simpnode_single(struct simpnode_t* node) {
	struct simpnode_t* temp;

	temp = node;

	if (temp->name)
		free(temp->name);

	if (temp->simpnode_type == SIMPNODE_DIRECTORY) {
		if (temp->simpnode.dir.children)
			free(temp->simpnode.dir.children);
		if (temp->simpnode.dir.raw_children)
			free(temp->simpnode.dir.raw_children);
	}

	free(temp);

	return;
}

int deinit_simpnode(struct simpnode_t* node,
		    void(*callback)(struct simpnode_t*, void*),
		    void* data) {
	stack_t stack;
	struct simpnode_t* curr;
	int i, ret;

	ret = 0;
	init_stack(&stack);

	if ((ret = push_simpnode(&stack, node)) < 0)
		return ret;

	while ((curr = pop_simpnode(&stack))) {

		if (callback)
			callback(curr, data);

		switch (curr->simpnode_type) {
		case SIMPNODE_DIRECTORY:
			for (i = 0; i <= curr->simpnode.dir.child_lst; i++) {
				if ((ret = push_simpnode(&stack,
					curr->simpnode.dir.children[i])) < 0) {
					deinit_stack(&stack);
					return ret;
				}
			}
			break;
		case SIMPNODE_FILE:
			free(curr->simpnode.file.data);
			break;
		default:
			return ret;
		}

		deinit_simpnode_single(curr);
	}

	deinit_stack(&stack);

	return ret;
}

static int process_dir(stack_t* stack, struct simpnode_t* curr) {
	DIR* dd;
	struct dirent* dentry;
	struct simpnode_t* newsn;
	char* curr_path;
	ssize_t cname_size;
	ssize_t ssize;
	int ret;

	ret = 0;
	curr_path = NULL;
	newsn = NULL;

	if (!(dd = opendir(curr->name))) {
		logs_err("Could not open %s", curr->name);
		return -EINVAL;
	}

	while ((dentry = readdir(dd))) {
		if (!strcmp(dentry->d_name, "."))
			continue;
		if (!strcmp(dentry->d_name, ".."))
			continue;

		cname_size = strlen(curr->name);
		ssize = strlen(dentry->d_name) + cname_size + 2;

		if (!curr_path)
			curr_path = malloc(sizeof(*curr_path) * ssize);
		else
			curr_path = realloc(curr_path,
					    sizeof(*curr_path) * ssize);

		strcpy(curr_path, curr->name);
		curr_path[cname_size] = '/';
		strcpy(&(curr_path[cname_size + 1]), dentry->d_name);
		curr_path[ssize - 1] = '\0';

		newsn = malloc(sizeof(*newsn));
		if (!newsn) {
			logs_err("Could not allocate memory.");
			return -ENOMEM;
		}

		newsn->level = curr->level + 1;

		switch (dentry->d_type) {
		case DT_UNKNOWN:
		case DT_REG:
			init_snfile(newsn, curr_path);
			break;
		case DT_DIR:
			init_sndir(newsn, curr_path);
			if ((ret = push_simpnode(stack, newsn)) < 0)
				return ret;
			break;
		default:
			break;
		}

		if ((ret = add_child(curr, newsn)) < 0)
			return ret;

	}

	if (curr_path)
		free(curr_path);

	closedir(dd);

	return 0;
}

static void print_node(struct simpnode_t* node) {
	int i = 0;

	for (i = 0; i < node->level * 5; i++)
		printf(" ");

	if (node->simpnode_type == SIMPNODE_DIRECTORY)
		printf("+ ");

	printf("%s\n", basename(node->name));

	return;
}

void print_tree(struct simpnode_t* root) {
	stack_t stack;
	struct simpnode_t* curr;
	int i;

	init_stack(&stack);

	if (push_simpnode(&stack, root) < 0) {
		logs_err("Error printing tree.");
		goto exit_print_tree;
	}

	while ((curr = pop_simpnode(&stack))) {
		print_node(curr);

		if (curr->simpnode_type != SIMPNODE_DIRECTORY)
			continue;

		for (i = 0; i <= curr->simpnode.dir.child_lst; i++) {
			if (push_simpnode(&stack,
			    curr->simpnode.dir.children[i]) < 0) {
				logs_err("Error printing tree.");
				goto exit_print_tree;
			}
		}
	}

exit_print_tree:

	deinit_stack(&stack);

	return;
}

struct simpnode_t* create_tree(const char* curr_dir) {
	char* cwd;
	struct simpnode_t* root;
	struct simpnode_t* curr;
	stack_t stack;

	cwd = NULL;
	root = NULL;

	cwd = curr_dir ? strdup(curr_dir) : strdup(getcwd(NULL, 0));
	if (!cwd) {
		logs_err("Error getting current working directory.");
		goto create_tree_fail;
	}

	root = malloc(sizeof(*root));
	if (!root) {
		logs_err("Could not allocate memory.");
		goto create_tree_fail;
	}

	init_sndir(root, cwd);
	init_stack(&stack);

	if (push_simpnode(&stack, root) < 0) {
		logs_err("Error pushing root node to stack.");
		goto create_tree_fail;
	}

	root->level = 0;

	while ((curr = pop_simpnode(&stack))) {
		process_dir(&stack, curr);
	}

	if (cwd)
		free(cwd);

	deinit_stack(&stack);

	return root;

create_tree_fail:

	if (cwd)
		free(cwd);

	if (root)
		deinit_simpnode(root, NULL, NULL);

	return NULL;
}

#define CHECK_DATA_LAST(DATA, OFF, ACT) \
	if ((DATA)->last + (OFF) >= (DATA)->size) { \
		logs_err("Data offset (%d) exceeds data size (%d).", (DATA)->last + (OFF), (DATA)->size); \
		ACT; \
	}

static int get_name_len(data_t* data) {
	int i;
       
	for (i = 0; i < data->size - data->last; i++) {
		CHECK_DATA_LAST(data, i, return -1);

		if (data->data[i + data->last] == '\0')
			return i;
	}

	return -1;
}

static uint32_t read_u32(data_t* data) {
	int i;
	uint32_t res;

	res = 0;

	for (i = 0; i < 4; i++)
		res |= ((uint32_t)data->data[data->last + i] >> ((4 - i - 1) * 8));

	return res;
}

static uint8_t read_u8(data_t* data) {

	return data->data[data->last];
}

#define SETUP_PARENT(RES, PAR, DATA, I) \
	for (I = 0; I < (PAR)->simpnode.dir.child_num; I++) { \
		if ((PAR)->simpnode.dir.raw_children[I] == (DATA)->last) { \
			(PAR)->simpnode.dir.children[I] = RES; \
			(RES)->level = (PAR)->level + 1; \
			break; \
		} \
	}

static struct simpnode_t* ds_dir(data_t* data, struct simpnode_t* parent) {
	struct simpnode_t* res;
	int i;

	res = NULL;

	res = malloc(sizeof(*res));
	if (!res) {
		logs_err("Could not allocate memory.");
		return NULL;
	}

	init_sndir(res, NULL);

	if (parent) {
		SETUP_PARENT(res, parent, data, i);
	}

	CHECK_DATA_LAST(data, 1, goto ds_dir_fail);
	if ((simpnode_type_t)read_u8(data) != SIMPNODE_DIRECTORY) {
		logs_err("Expected directory. Possible data corruption (%d).", (int)read_u8(data));
		goto ds_dir_fail;
	}

	data->last++;

	res->name_size = get_name_len(data);
	if (res->name_size < 0) {
		logs_err("Could not extract directory name.");
		goto ds_dir_fail;
	}

	res->name = strndup((char*)&(data->data[data->last]), res->name_size);
	if (!res->name) {
		logs_err("Could not extract directory name.");
		goto ds_dir_fail;
	}

	data->last += res->name_size + 1;

	CHECK_DATA_LAST(data, 4, goto ds_dir_fail);
	res->simpnode.dir.child_num = (int)read_u32(data);
	res->simpnode.dir.child_lst = res->simpnode.dir.child_num - 1;

	if (res->simpnode.dir.child_num > 100)
		exit(1);

	data->last += 4;

	res->simpnode.dir.raw_children = malloc(
				sizeof(*(res->simpnode.dir.raw_children)) *
				res->simpnode.dir.child_num);
	res->simpnode.dir.children = malloc(
				sizeof(*(res->simpnode.dir.children)) *
				res->simpnode.dir.child_num);


	for (i = 0; i < res->simpnode.dir.child_num; i++) {
		CHECK_DATA_LAST(data, 4, goto ds_dir_fail);
		res->simpnode.dir.raw_children[i] = read_u32(data);
		data->last += 4;
	}

	return res;

ds_dir_fail:

	if (res) {
		res->simpnode_type = SIMPNODE_DIRECTORY; /* we need this */
		deinit_simpnode_single(res);
	}

	return NULL;
}

static struct simpnode_t* ds_file(data_t* data, struct simpnode_t* parent) {
	struct simpnode_t* res;
	int i;

	res = NULL;

	res = malloc(sizeof(*res));
	if (!res) {
		logs_err("Could not allocate memory.");
		return NULL;
	}

	init_snfile(res, NULL);

	if (parent) {
		SETUP_PARENT(res, parent, data, i);
	}

	CHECK_DATA_LAST(data, 1, goto ds_file_fail);
	if ((simpnode_type_t)read_u8(data) != SIMPNODE_FILE) {
		logs_err("Expected file. Internal bug.");
		goto ds_file_fail;
	}

	data->last++;

	res->name_size = get_name_len(data);
	if (res->name_size < 0) {
		logs_err("Could not extract directory name.");
		goto ds_file_fail;
	}

	res->name = strndup((char*)&(data->data[data->last]), res->name_size);
	if (!res->name) {
		logs_err("Could not extract file name.");
		goto ds_file_fail;
	}

	data->last += res->name_size + 1;

	CHECK_DATA_LAST(data, 4, goto ds_file_fail);
	res->simpnode.file.data_size = (int)read_u32(data);

	data->last += 4;

	if (res->simpnode.file.data_size > 0) {
		res->simpnode.file.data = malloc(
					sizeof(*(res->simpnode.file.data)) *
					res->simpnode.file.data_size);

		memcpy(res->simpnode.file.data, &(data->data[data->last]),
		       res->simpnode.file.data_size);
	}
	else {
		res->simpnode.file.data = NULL;
	}

	data->last += res->simpnode.file.data_size;


	return res;

ds_file_fail:

	if (res) {
		res->simpnode_type = SIMPNODE_FILE; /* if data corrupted */
		deinit_simpnode_single(res);
	}

	return NULL;
}

struct simpnode_t* deserialize(const char* infile) {
	struct simpnode_t* root;
	struct simpnode_t* curr;
	struct simpnode_t* news;
	stack_t stack;
	int i;
	data_t data; /* we are using data_t a bit differently here */

	init_data(&data);
	init_stack(&stack);

	root = NULL;

	data.data = load_file(infile, (unsigned int*)&(data.size));
	if (!data.data)
		return NULL;

	if (data.size >= 1024)
		return NULL;

	data.last = 0;

	root = ds_dir(&data, NULL);

	if (push_simpnode(&stack, root) < 0) {
		logs_err("Error in stack");
		root = NULL;
		goto exit_deserialize;
	}

	while ((curr = pop_simpnode(&stack))) {

		if (data.last >= data.size) {
			logs_err("Possible data corruption.");
			root = NULL;
			goto exit_deserialize;
		}

		for (i = 0; i < curr->simpnode.dir.child_num; i++) {
			data.last = curr->simpnode.dir.raw_children[i];

			switch ((int)read_u8(&data)) {

			case SIMPNODE_FILE:

				if (!(news = ds_file(&data, curr))) {
					root = NULL;
					goto exit_deserialize;
				}

				break;

			case SIMPNODE_DIRECTORY:

				if (!(news = ds_dir(&data, curr))) {
					root = NULL;
					goto exit_deserialize;
				}

				if (push_simpnode(&stack, news) < 0) {
					logs_err("Could not push to stack.");
					root = NULL;
					goto exit_deserialize;
				}

				break;

			default:
				logs_err("Unexpected value at %d (%.2x)",
					 data.last, data.data[data.last]);
				root = NULL;
				goto exit_deserialize;

			}
		}
	}

exit_deserialize:

	if (data.data)
		free(data.data);
	deinit_stack(&stack);

	return root;
}

struct simpnode_t* get_node(const char* infile, const char* path) {
	char** path_arr;
	char* path_dup;
	char* token;
	int i, j, path_arr_sz;
	bool has_root;
	ssize_t len;
	struct simpnode_t* res;
	struct simpnode_t* root;

	stack_t stack;

	path_arr = NULL;
	path_dup = NULL;
	token = NULL;
	res = NULL;
	root = NULL;
	i = 0;
	has_root = path[0] == '/';
	path_arr_sz = 1;

	init_stack(&stack);

	len = strlen(path);

	path_dup = strdup(path);
	if (!path_dup) {
		logs_err("Could not allocate memory.");
		return NULL;
	}

	if (path_dup[len - 1] == '/')
		path_dup[len - 1] = '\0';

	while (path_dup[i] != '\0') {

		if (path_dup[i] == '/')
			path_arr_sz++;
		i++;
	}

	printf("Array size: %d\n", path_arr_sz);

	path_arr = malloc(sizeof(*path_arr) * path_arr_sz);
	if (!path_arr) {
		logs_err("Could not allocate memory.");
		goto get_node_exit;
	}

	i = 0;
	token = strtok(path_dup, "/");

	if (has_root) {
		printf("Has root\n");
		path_arr[0] = strdup("root");
		token = strtok(NULL, "/");
		i++;
	}

	while (token != NULL) {
		path_arr[i] = strdup(token);
		printf("%s (%d) = %s\n", path_arr[i], i, token);

		if (!path_arr[i]) {
			logs_err("Could not allocate memory.");
			goto get_node_exit;
		}

		token = strtok(NULL, "/");
		i++;
	}

	printf("Tokenized.\n");

	root = deserialize(infile);
	if (!root) {
		logs_err("Could not deserialize %s", infile);
		goto get_node_exit;
	}

	printf("Deserialized.\n");

	if (push_simpnode(&stack, root) < 0) {
		logs_err("Could not push node.");
		goto node_not_found;
	}

	i = 0;

	if (strcmp(root->name, path_arr[i])) {
		printf("Comparing %s with %s\n", root->name, path_arr[0]);
		res = NULL;
		goto node_not_found;
	}

	i++;

	while ((res = pop_simpnode(&stack))) {

		if (i == path_arr_sz) {
			goto get_node_exit;
		}
		else if (res->simpnode_type == SIMPNODE_FILE) {
			continue;
		}
		else if (res->simpnode_type == SIMPNODE_DIRECTORY) {
			printf("Here\n");
		
			for (j = 0; j <= res->simpnode.dir.child_lst; j++) {
				printf("Comparing %s with %s\n", res->simpnode.dir.children[j]->name,
					   path_arr[i]);
				if (strcmp(res->simpnode.dir.children[j]->name,
					   path_arr[i]))
					continue;

				if (push_simpnode(&stack,
					res->simpnode.dir.children[j]) < 0) {
					logs_err("Could not push node.");
					goto node_not_found;
				}
				printf("Pushed %s\n", res->simpnode.dir.children[j]->name);
				break;
			}
			i++;
		}
		else {
			logs_err("Unknown state.");
			res = NULL;
			goto node_not_found;
		}
	}

	goto get_node_exit;

node_not_found:

	deinit_simpnode(root, NULL, NULL);

get_node_exit:

	if (path_dup)
		free(path_dup);

	if (path_arr) {
		for (i = 0; i < path_arr_sz; i++)
			if (path_arr[i])
				free(path_arr[i]);

		free(path_arr);
	}

	deinit_stack(&stack);

	return res;
}
