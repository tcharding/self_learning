/* Source: page 49 KnR Second Edition */

/* getbits: get n bits from position p */
unsigned gitbits(unsigned x, int p, int n)
{
	return (x >> (p+1-n)) & ~(~0 << n);
}
