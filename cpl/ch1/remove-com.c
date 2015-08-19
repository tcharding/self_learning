#include <stdio.h>
#include <stdlib.h>

/* 
 * remove comments from C source file
 * use only: putchar() and getchar()
 * no pointers
 */

enum { IN, OUT };

static char *test = "string with /* comment */ string in it";

void skip(void);

int main(void) 
{
	int c, d;
	int state;		/* IN if within string */

	state = OUT;
	while ( (c = getchar()) != EOF) {
		if ( c == '"')	
			if (state == OUT) { /* start string */
				state = IN;
				putchar(c);
				continue;
			} else { /* end string */
				state = OUT;
				putchar(c);
				continue;
			}
		if (state == IN) { /* within string*/
				putchar(c);
				continue;
		}
		
		if ( c != '/' ) {
			putchar(c);
			continue;
		}
		if ( (d = getchar()) == EOF) {
			putchar(c);
			exit(0);
		} else if (d != '*') {
			putchar(c); 
			putchar(d);
			continue;
		}
		skip();		/* skip comments */
	}
}

/* skip: skip over comments */
void skip(void)
{
	int c, d;
	
	while ( (c = getchar()) != EOF) {
		if ( c != '*' ) {
			continue;
		}
		if ( (d = getchar()) == EOF) {
			exit(0);
		} else if (d != '/') {
			continue;
		}
		return;		/* end of comment section */
	}
}
