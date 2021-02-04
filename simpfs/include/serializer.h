#ifndef SERIALIZER_H
#define SERIALIZER_H

#include <stdint.h>

typedef struct {
	uint8_t* data;
	int size;
	int last;
} data_t;

void init_data(data_t* data);
void deinit_data(data_t* data);
data_t* serialize(const char* curr_dir, const char* outfile);

#endif
