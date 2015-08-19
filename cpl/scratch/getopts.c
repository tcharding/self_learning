#include <stdio.h>
/*  attr: Kernighan and Ritchie, second Edition */
int main(int argc, char *argv[])
{
	int c, opt;

	opt = 0;
	while (--argc > 0 && **++argv == '-') {
		while ( (c = *++argv[0])) {
			switch (c) {
			case 'o':
				numeric = 1;
				break;
			default:
				printf("sort: illegal option %c\n", c);
				argc = 0;
				break;
			}
		}
	}
	printf("option: %d\n", opt);
	return 0;
}
