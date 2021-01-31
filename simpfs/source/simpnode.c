#include "simpnode.h"

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <errno.h>

#include <sys/types.h>	/* DIR */
#include <dirent.h>	/* opendir() */
#include <unistd.h>	/* getcwd() */

#define logs_err(fmt, ...) printf("(!) " fmt "\n", ## __VA_ARGS__)
#define logs_inf(fmt, ...) printf("(i) " fmt "\n", ## __VA_ARGS__)

static void init_sndir(struct simpnode_t* node, const char* name) {

	node->name = strdup(name);
	node->simpnode_type = SIMPNODE_DIRECTORY;
	node->simpnode.dir = (simpnode_dir_t) {
		.child_num = 0,
		.child_lst = -1,
		.children = NULL
	};

	return;
}

static uint8_t* load_file(const char* infile, unsigned int* len) {
	uint8_t* out;
	long fsize;
	FILE* f;

	f = fopen(infile, "r");
	if (!f) {
		logs_err("Could not open file %s", infile);
		return NULL;
	}

	fseek(f, 0, SEEK_END);

	fsize = ftell(f);

	out = malloc(fsize);
	if (!out) {
		logs_err("Could not allocate memory.");
		return NULL;
	}

	*len = fsize;

	fseek(f, 0, SEEK_SET);

	if (fread(out, 1, fsize, f) != fsize) {
		free(out);
		logs_err("Error reading file %s", infile);
		return NULL;
	}

	fclose(f);

	return out;
}

static int init_snfile(struct simpnode_t* node, const char* name) {
	uint8_t* data;
	unsigned int data_size;

	node->name = strdup(name);
	if (!node->name) {
		logs_err("Error executing strdup().");
		return -ENOMEM;
	}

	data = load_file(name, &data_size);
	if (!data)
		return -EINVAL;

	node->simpnode_type = SIMPNODE_FILE;
	node->simpnode.file = (simpnode_file_t) {
		.data_size = data_size,
		.data = data
	};

	return 0;
}

#define DIRPR(NODE, PROP) \
	(NODE)->simpnode.dir.PROP

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

typedef struct {
	struct simpnode_t** stack;
	int last;
	int size;
} stack_t;

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

static const char* basename(const char* path) {
	unsigned int i;

	for (i = strlen(path); i >= 0; i--)
		if (path[i] == '/')
			return &(path[i]);

	return path;
}

static void deinit_simpnode_single(struct simpnode_t* node) {
	struct simpnode_t* temp;

	temp = node;

	logs_inf("Freeing %s (%s)", temp->name,
		 temp->simpnode_type == SIMPNODE_DIRECTORY ? "D" : "F");
	free(temp->name);

	if (temp->simpnode_type == SIMPNODE_DIRECTORY)
		free(temp->simpnode.dir.children);

	free(temp);

	return;
}

int deinit_simpnode(struct simpnode_t* node) {
	stack_t stack;
	struct simpnode_t* curr;
	int i, ret;

	ret = 0;
	init_stack(&stack);

	if ((ret = push_simpnode(&stack, node)) < 0)
		return ret;

	while ((curr = pop_simpnode(&stack))) {

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

struct simpnode_t* create_tree(const char* curr_dir) {
	char* cwd;
	struct simpnode_t* root;
	struct simpnode_t* curr;
	stack_t stack;

	cwd = NULL;
	root = NULL;

	cwd = curr_dir ? strdup(curr_dir) : getcwd(NULL, 0);
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
		deinit_simpnode(root);

	return NULL;
}

