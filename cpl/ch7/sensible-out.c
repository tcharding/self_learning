#include <stdio.h>
#include <ctype.h>
#include <string.h>
#define LINEMAX 80

/* print arbitrary input in sensible manner */
int main(int argc, char *argv[])
{
	int c, cnt;

	cnt = 0;
	while ((c = getchar()) != EOF) {
		cnt++;
		if (cnt > LINEMAX) {
			putchar('\n');
			putchar('>');
			cnt = 0;
		}
		if (isprint(c))
			printf("%c", c);
		else if ( c == '\n' || c == '\t')
			printf("%c", c);
		else
			printf("%X", c);
	}
	return 0;
}
