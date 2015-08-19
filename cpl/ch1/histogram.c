#include <stdio.h>

enum {
	IN,			/* state: in word */
	OUT,			/* state: out of word */
	MAX = 16		/* max word length */
};

/* print histogram of lengths of words in input */
int main(void)
{
	int c, state, len, i, j;


	int index[MAX];
	
	len = 0;
	state = OUT;
	for (i = 0; i < MAX; i++)
		index[i] = 0;

	while ( (c = getchar()) != EOF) {
		if (c == ' ' || c == '\n' || c == '\t') {
			if (state == IN) {
				len = (len >= MAX) ? MAX : len;
				++index[len-1];
				len = 0;
			}
			state = OUT;
			continue;
		}
		state = IN;
		++len;
	}
	for (i = 0; i < MAX; i++) {
		printf("  %d\t", i+1);
		for (j = 0; j < index[i]; j++)
			printf("*");
		printf("\n");
	}
	return 0;
}
