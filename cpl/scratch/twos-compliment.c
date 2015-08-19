#include <stdio.h>

void bitstring(char buf[], unsigned x, int wordlength);

/* play with two's compliment number system */
int main(void)
{
	char n;
	char buf[9];
	
	n = -128;
	bitstring(buf, n, 8);
	printf("%d: %s\n", n, buf);
	n = 127;
	bitstring(buf, n, 8);
	printf("%d: %s\n", n, buf);
	n = -1;
	bitstring(buf, n, 8);
	printf("%d: %s\n", n, buf);

	n = -128;
	n = -n;
	bitstring(buf, n, 8);
	printf("%d: %s\n", n, buf);

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
