#include <stdio.h>

/* verify that (getchar() != EOF) is 0 or 1 */
int main(void)
{
	int res;
	
	for ( ; ; ) {
		res = (getchar() != EOF);
		printf("val: %d\n", res);
	}
	return 0;
}
