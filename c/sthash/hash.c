#include "st.h"
#include <stdlib.h>
#include <stdio.h>

int main(int argc, char **argv) {

	st_table *table; int i;
	table = st_init_table(&type_strhash);
	for (i = 0; i < 1000000; i++) {
		char *key = (char *)malloc(32);
		sprintf(key, "%d", i);
		st_insert(table, key, "2");
	}
	return 0;
}
