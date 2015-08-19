#include <stdio.h>
#include <string.h>

/*
 * if n is char type -128 < n < 127
 */ 
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
	unsigned u;

	sign = n;		/* record sign */
	/* make it positive */
	/* 2's compliment number range = -(max+1) -> max */
	u = (n < 0) ? n * -1 : n;
	
	i = 0;
	do {			/* generate digits in reverse order */
		s[i++] = u % 10 + '0'; /* get next digit */
	} while ( (u /= 10) > 0);      /* delete it */
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
