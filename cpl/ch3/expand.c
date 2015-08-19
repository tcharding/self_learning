#include <stdio.h>

enum { MAXLINE = 4096 };

void expand(char dst[], char src[]);

int main(void)
{
	char readln[MAXLINE], buf[MAXLINE];

	while (fgets(readln, MAXLINE, stdin) != NULL) {
		expand(buf, readln);
		fprintf(stdout, "%s\n", buf);
	}
	return 0;
}

/* expand: expand shorthand a-g into abcdefg */
void expand(char dst[], char src[])
{
	int i, j;		/* loop counters */
	int c, end;		

	for (i = 0, j = 0; src[i] != '\0'; i++, j++) {
		if (src[i+1] == '-') {
			c = src[i];
			end = src[i+2];
			for ( ; c != end; c++, j++) {
				dst[j] = c;
			}
			dst[j] = end;
			i += 2;	/* skip over processed shorthand */
		} else {
			dst[j] = src[i];
		}
	}
	dst[j] = '\0';
}
