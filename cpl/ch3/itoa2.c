#include <stdio.h>
#include <string.h>

enum { BUFSIZE = 4096 };

void reverse(char s[]);
void itoa(int n, char s[], int width);

int main(void)
{
	int n;
	char buf[BUFSIZE];

	n = -2147483648;
	itoa(n, buf, 20);
	fprintf(stderr, "int val: %d\n", n);
	fprintf(stderr, "20-space-pad-*******\n");
	fprintf(stderr, "%s\n", buf);
	return 0;
}

/* itoa: convert n to characters in s, pad with spaces to width w */
void itoa(int n, char s[], int w)
{
	int i, sign, pad, ndigits;
	unsigned u;

	sign = n;		  /* record sign */
	ndigits = 0;
	u = (n < 0) ? n * -1 : n; /* make it positive */
	i = 0;
	do {			/* generate digits in reverse order */
		s[i++] = u % 10 + '0'; /* get next digit */
		++ndigits;
	} while ( (u /= 10) > 0);      /* delete it */
	if (sign < 0) {
		s[i++] = '-';
		++ndigits;
	}
	pad = w - ndigits;
	for ( ; pad > 0; pad--) 
		s[i++] = ' ';
	s[i] = '\0';
	reverse(s); 
}

/*  attr: Kernighan and Ritchie, second Edition page 62 */
/* reverse: reverse string s in place */
void reverse(char s[])
{
	int c, i, j;

	for (i = 0, j = strlen(s)-1; i < j; i++, j--) {
		c = s[i];
		s[i] = s[j];
		s[j] = c;
	}
}
