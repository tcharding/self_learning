#include <stdio.h>

enum { OFF, ON };

/* squash string of blanks into single blank character ; version 2 */
int main(void)
{
	int c;
	int flag;		/* ON if last char was 'space' */

	flag = OFF;
	while ( (c = getchar()) != EOF) {
		if ( (c == ' ') && (flag == ON))
			continue;

		flag = (c == ' ') ? ON : OFF;
		putchar(c);
	}
	return 0;
}
