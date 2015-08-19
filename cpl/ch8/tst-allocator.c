#include <stdio.h>

void *malloc(unsigned nbytes);
void free(void *ap);
void *calloc(int n, unsigned s);

/* test allocator implementation */
int main()
{
	char *buf;
	unsigned size;
	int *p, n;
	
	size = 1;
	if ((buf = malloc(size)) == NULL) 
		fprintf(stderr, "main: malloc failed: %u\n", size);
	free(buf);

	size = 2048;
	if ((buf = malloc(size)) == NULL) 
		fprintf(stderr, "main: malloc failed: %u\n", size);
	free(buf);

	n = 1;
	size = 2048;
	if ((p = calloc(n, size)) == NULL) 
		fprintf(stderr, "main: calloc failed: %d * %u\n", n, size);
	free(p);
	
	n = 10;
	size = 2048;
	if ((p = calloc(n, size)) == NULL) 
		fprintf(stderr, "main: calloc failed: %d * %u\n", n, size);
	free(p);
	

	return 0;
}
