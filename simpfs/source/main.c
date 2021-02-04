#include <stdio.h>
#include <stdbool.h>
#include <getopt.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>

#include "common.h"
#include "serializer.h"
#include "simpnode.h"

#define COLS 16

#ifdef SIMP_DEBUG
static bool printable(char c) {

	return !(c < 32 || c > 125);
}

void dump_data(const data_t* data) {
	int i;

	for (i = 0; i < data->last; i++) {
		if (i % COLS == 0) {
			printf("[%.3d]: ", i);
		}
		if (printable((char)data->data[i]))
			printf(" %c ", (char)data->data[i]);
		else
			printf("%.2x ", data->data[i]);
		if (i % COLS == COLS - 1)
			printf("\n");
	}

	printf("\n");

	return;
}
#endif

typedef enum {
	MAIN_ACTION_INSP,
	MAIN_ACTION_PACK,
	MAIN_ACTION_LIST,
	MAIN_ACTION_HELP,
	MAIN_ACTION_NONE
} main_action_t;

static struct option long_options[] = {
	{ "inspect",		required_argument,	0, 'i' },
	{ "list",		no_argument,		0, 'l' },
	{ "archive-to",		required_argument,	0, 'a' },
	{ "help",		no_argument,		0, 'h' },
	{ 0,			0,			0,  0  }
};

#define SET_ACTION(ACT) \
	if (action != MAIN_ACTION_NONE) { \
		logs_err("Invalid use of parameters. Try -h for help."); \
		ret = -EINVAL; \
		goto main_exit; \
	} else { \
		action = ACT; \
	}

int main(int argc, char* const* argv) {
	data_t* data;
	int opt, option_index, ret;
	struct simpnode_t* root;
	main_action_t action;
	char* inf;
	char* outf;

	if (argc < 2) {
		logs_err("No arguments provided. Try -h for help.");
		return -EINVAL;
	}

	inf = argv[1][0] != '-' ? argv[1] : NULL;

	ret = 0;
	option_index = 0;
	outf = NULL;
	action = MAIN_ACTION_NONE;

	while ((opt = getopt_long(inf ? argc -1 : argc,
				  inf ? &(argv[1]) : argv, "i:la:h",
				  long_options, &option_index)) != -1) {
		switch (opt) {
		case 'i':
			SET_ACTION(MAIN_ACTION_INSP);
			outf = optarg;
			break;
		case 'l':
			SET_ACTION(MAIN_ACTION_LIST);
			break;
		case 'a':
			SET_ACTION(MAIN_ACTION_PACK);
			outf = optarg;
			break;
		case 'h':
			SET_ACTION(MAIN_ACTION_HELP);
			break;
		default:
			break;

		}
	}

	switch (action) {
	case MAIN_ACTION_INSP:
		root = get_node(inf, outf);
		if (root)
			printf("Found.\n");
		else
			printf("Not found.\n");
		break;

	case MAIN_ACTION_LIST:
		root = deserialize(inf);
		print_tree(root);
		deinit_simpnode(root, NULL, NULL);
		break;

	case MAIN_ACTION_PACK:
		data = serialize(inf, outf);
		printf("(i) Created image %s (%d bytes).\n",
		       outf, data->last + 1);
		deinit_data(data);
		break;

	case MAIN_ACTION_HELP:
		printf("simpfs [archive|folder] <-a <outf>|-l>\n");
		break;

	default:
		logs_err("No valid action provided. Try -h for help.");
		ret = -EINVAL;
		goto main_exit;
	}

main_exit:

	return ret;
}
