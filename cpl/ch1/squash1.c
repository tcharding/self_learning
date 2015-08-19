#include <stdio.h>

/* squash string of blanks into single blank character ; version 1 */
int main(void)
{
	int c;
	int flag;		/* 1 if last char was 'space' */

	flag = 0;
	while ( (c = getchar()) != EOF) {
		if ( (c == ' ') && (flag == 1))
			continue;

		if (c == ' ')
			flag = 1;
		else
			flag = 0;
		
		putchar(c);
	}
	return 0;
}
