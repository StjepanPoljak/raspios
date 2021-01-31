#ifndef SIMPNODE_H
#define SIMPNODE_H

#include <stdint.h>

typedef enum {
	SIMPNODE_DIRECTORY,
	SIMPNODE_FILE,
} simpnode_type_t;

typedef struct {
	unsigned int data_size;
	uint8_t* data;
} simpnode_file_t;

typedef struct {
	int child_num;
	int child_lst;
	struct simpnode_t** children;
} simpnode_dir_t;

union simpnode_ut {
	simpnode_file_t file;
	simpnode_dir_t dir;
};

struct simpnode_t {
	int name_size;
	char* name;
	simpnode_type_t simpnode_type;
	union simpnode_ut simpnode;
};

struct simpnode_t* create_tree(const char* curr_dir);
int deinit_simpnode(struct simpnode_t* node,
		    void(*callback)(struct simpnode_t*, void*),
		    void* data);

#endif
