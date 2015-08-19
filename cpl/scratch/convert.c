#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <stdint.h>

enum {
	CHAR = 8,
	SHORT = 16,
	INT = 32,
	LONG = 64,
	BUFSIZE = 1024
};

void bitstring(char buf[], unsigned x, int wordlength);

/* print argv[1] in binary form */
int main(int argc, char *argv[])
{
	int base = 2;		/* default to binary */
	int n;
	char buf[BUFSIZE];
	
	if (argc < 2) {
		printf("Usage: %s [-b base] number\n", *argv);
		return 1;
	}
	n = atoi(*++argv); 
	printf("converting n = %d to base %d\n", n, base);
	bitstring(buf, n, CHAR); 
	printf("%s\n", buf);
	return 0;
}

/* bitstring: pretty print binary */
void bitstring(char buf[], unsigned x, int wordlength)
{ 
	int i;

	for ( i = wordlength; i > 0; i--) {
		if (x & 01)
			buf[i-1] = '1';
		else
			buf[i-1] = '0';
		x >>= 1;
	}
	buf[wordlength] = '\0';
	return;
}
