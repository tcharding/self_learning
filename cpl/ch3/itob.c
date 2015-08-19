#include <stdio.h>
#include <string.h>

enum{
	BUFSIZE = 4096,
	OCT = 8,
	HEX = 16,
	B64 = 64
};

int base[] = {2, 4, 8, 10, 16, 64};
char *sval[] = {		
	"111110011100111",	/* BASE 2  */
	"13303213",		/* BASE 4 */
	"76347",		/* BASE 8 */
	"31975",		/* BASE 10 */
	"7CE7",			/* BASE 16 */
	"Hzn",			/* BASE 64  */
	NULL
};
	
void itob(unsigned n, char s[], int b);
int itod(int n, int b);
void reverse(char s[]);

int main(void)
{
	char buf[BUFSIZE];
	int n, i;

	n = 0x7CE7;
	for (i = 0; sval[i] != NULL; i++) {
		itob(n, buf, base[i]);
		if (strcmp(sval[i], buf) != 0)
			fprintf(stderr, "itob failed: base: %d sval: %s res: %s\n",
				base[i], sval[i], buf);
	}
	return 0;
}
/* itob: convert n into characters in s in base b */
void itob(unsigned n, char s[], int b)
{
	int i;

	i = 0;
	do {			/* generate digits in reverse */
		s[i++] = itod((n % b), b);
	} while ((n /= b) > 0);
	s[i] = '\0';
	reverse(s);
}

/* itod: convert n to digit in base b */
int itod(int n, int b)
{
	char hex[] = "0123456789ABCDEF";
	char base64[] =
		"ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";
	char *digits;
	
	if (b <= 16) 
		digits = hex;
	else if (b == 64)
		digits = base64;
	else
		return -1;	/* base not supported */

	return digits[n];
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
