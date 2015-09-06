#include "tch.h"

/*
 * C test framework
 */

void tf_equ(int a, int b, int line)
{
	if (a != b)
		fprintf(stderr, "test failed line: %d (a: %d b: %d)\n",
			line, a, b);
}
