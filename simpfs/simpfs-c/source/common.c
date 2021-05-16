#include "common.h"

#include <string.h>
#include <stdlib.h>

uint8_t* load_file(const char* infile, unsigned int* len) {
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


const char* basename(const char* path) {
	int i;

	for (i = strlen(path); i >= 0; i--)
		if (path[i] == '/')
			return &(path[i + 1]);

	return path;
}
