#include <stdio.h>

enum { BUFSIZE = 4096 };

void itoa(int n, char s[]);

int main(void)
{
	int n;
	char buf[BUFSIZE];

	n = 2147483648;
	itoa(n, buf);
	fprintf(stderr, "n: %d\tstring: %s\n", n, buf);
	return 0;
}

/* r_itoa: recursive helper */
void r_itoa(unsigned int n, char s[])
{

	if (n / 10)
		r_itoa((n / 10), &s[0]);
	s[1] = n % 10 + '0';
}

/* itoa: convert n to characters in s */
void itoa(int n, char s[])
{
	int i;
	unsigned u;
	
	i = 0;
	if (n < 0) {
		s[i++] = '-';
		u = -1 * n;
	} else
		u = n;
	r_itoa(u, &s[i]);
}

