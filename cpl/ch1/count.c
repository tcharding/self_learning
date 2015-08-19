#include <stdio.h>

/* count blank lines, tabs, and new lines */
int main(void)
{
	int c, bl, t, nl;
	int flag;		/* 1 if last char was a new line */

	bl = t = nl = flag = 0;
	while ( (c = getchar()) != EOF) {
		if (c == '\t')
			++t;
		if (c == '\n') { 
			++nl;
			if (flag == 1)
				++bl;
			flag = 1;
		} else
			flag = 0;
	}
		
	printf("tabs: %d\tnew lines: %d\tblank lines: %d\n", t, nl, bl);

	return 0;
}
