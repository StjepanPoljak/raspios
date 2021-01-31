#include <stdio.h>
#include <stdbool.h>

#include "serializer.h"

#define COLS 16

bool printable(char c) {

	return !(c < 32 || c > 125);
}

int main(int argc, const char* argv[]) {
	data_t* data;
	int i;

	data = serialize(NULL);

	printf("Got %d bytes.\n", data->last + 1);

	for (i = 0; i < data->last; i++) {
		if (printable((char)data->data[i]))
			printf(" %c ", (char)data->data[i]);
		else
			printf("%.2x ", data->data[i]);
		if (i % COLS == COLS - 1)
			printf("\n");
	}

	printf("\n");

	return 0;
}
