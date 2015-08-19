#include <stdio.h>
#include <string.h>

enum { BUFSIZE = 4096 };

void reverse(char s[]);
void itoa(int n, char s[]);

int main(void)
{
	int n;
	char buf[BUFSIZE];

	n = -2147483648;
	itoa(n, buf);
	fprintf(stderr, "n: %d\tstring: %s\n", n, buf);
	return 0;
}

/* itoa: convert n to characters in s */
void itoa(int n, char s[])
{
	int i, sign;

	if ((sign = n) < 0) {	/* record sign */
		n = -n;
		/* 
		 * in two's compliment number system above statement
		 * fails for n = max negative for type i.e 10...0 
		 */
	}
	i = 0;
	do {			/* generate digits in reverse order */
		s[i++] = n % 10 + '0'; /* get next digit */
	} while ( (n /= 10) > 0);      /* delete it */
	if (sign < 0)
		s[i++] = '-';
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
