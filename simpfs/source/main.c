#include <stdio.h>

#include "simpnode.h"

int main(int argc, const char* argv[]) {
	struct simpnode_t* root;

	root = create_tree(NULL);

	deinit_simpnode(root);

	return 0;
}
