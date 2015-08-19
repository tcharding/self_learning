#include <stdio.h>
#include <stdint.h>

enum {
	CHAR = 8,
	SHORT = 16,
	INT = 32,
	LONG = 64
};

void bitstring(char buf[], unsigned x, int wordlength);
unsigned getbits(unsigned x, int p, int n);
unsigned setbits(unsigned x, int p, int n, unsigned y);
unsigned invert(unsigned x, int p, int n);
unsigned rightrot(unsigned x, int places, int wordlength);
int bitcount(unsigned x);

int main(void)
{
	unsigned int x, y, z;
	char bin[INT+1];

	/* test getbits */
	printf("testing getbits()\n");
	x = 1751262053;
	bitstring(bin, x, INT);
	printf("x: %s\n", bin);

	y = getbits(x, 31, 8);
	bitstring(bin, y, INT);
	printf("low order byte: %s\n", bin);
	y = getbits(x, 7, 8);
	bitstring(bin, y, INT);
	printf("high order byte: %s\n", bin);
	
	/* test setbits */
	printf("\ntesting setbits()\n");
	x = 1751262053;
	bitstring(bin, x, INT);
	printf("x: %s\n", bin);

	y = 0x55;
	bitstring(bin, y, INT);
	printf("y: %s\n", bin);
	
	z = setbits(x, 7, 8, y);
	bitstring(bin, z, INT);
	printf("z: %s\n", bin);
	/* test invert() */
	printf("\ntesting invert()\n");
	printf("invert(x, 15, 8)\n");
	x = 1751262053;
	bitstring(bin, x, INT);
	printf("x: %s\n", bin);
	z = invert(x, 15, 8);
	bitstring(bin, z, INT);
	printf("z: %s\n", bin);

	/* test rightrot() */
	printf("\ntesting rightrot()\n");
	printf("rightrot(x, 4)\n");
	x = 1751262053;
	bitstring(bin, x, INT);
	printf("x: %s\n", bin);
	z = rightrot(x, 4, INT);
	bitstring(bin, z, INT);
	printf("z: %s\n", bin);

	/* test bitcount */
	printf("\ntesting bitcount()\n");
	x = 0x5555;
	bitstring(bin, x, INT);
	printf("%s: %d\n", bin, bitcount(x));
	
	return 0;
}
/* invert: invert n bits starting at position p */
unsigned invert(unsigned x, int p, int n)
{
	unsigned mask;

	mask = ~(~0 << n) << (p + 1 - n);
	return (x ^ mask);
}
/* setbits: set n bits of x starting at p to low order n bits of y */
unsigned setbits(unsigned x, int p, int n, unsigned y)
{
	unsigned mask;

	y &= ~(~0 << n);	/* mask off high order bits */
	y <<= (p + 1 - n);	/* shift to correct place */
	mask = ~(~(~0 << n) << (p + 1 - n)); /* string of 0's in correct place */
	x &= mask;			     /* mask off bits we are changing */
	return x | y;
}

/* rightrot: rotate x n places to the right */
unsigned rightrot(unsigned x, int n, int wordlength)
{
	unsigned left, right, mask;  

	left = right = x;
	mask = ~0 << n;
	left &= mask;
	mask = ~(~0 << n);
	right &= mask;

	/* now swap them */ 
	left >>= n;
	right <<= wordlength - n;
	return (left | right);	/* combine */
}
/* bitcount: count 1 bits in x */
int bitcount(unsigned x)
{
	int b;

	for (b = 0; x != 0; b++)
		x &= x-1;
	return b;
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

/* attr: Kernighan and Ricthie Second Edition */
/* getbits: get n bits from position  */
unsigned getbits(unsigned x, int p, int n)
{
	return (x >> (p+1-n)) & ~(~0 << n);
}

