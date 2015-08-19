#include <stdio.h>

enum { IN, OUT };

/* print input one word per line  */
int main(void)
{
	int c, state;

	state = OUT;
	while ( (c = getchar()) != EOF) {
		if (c == ' ' || c == '\n' || c == '\t') {
			if (state == IN) 
				putchar('\n');
			state = OUT;
			continue;
		}
		state = IN;
		putchar(c);
	}
	return 0;
}
