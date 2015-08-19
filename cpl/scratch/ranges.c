#include <stdio.h>
#include <limits.h>

/*
 * determine range of standard types 
 */

void range_char(void);
void range_short(void);
void range_int(void);
void range_long(void);

int main(void)
{
	range_char();
	range_short();
	range_int();
	range_long();

	return 0;
}

/* range_char: print char type value ranges */
void range_char(void)
{
	unsigned char max, umax;
	char min;
	int BITS = 8;

	min = CHAR_MIN;	
	max = CHAR_MAX;
	umax = UCHAR_MAX;
	printf("limit.h: char\n");
	printf(" %19d\tmin\n%20d\tmax\n%20u\tumax\n", min, max, umax);
	
	min = ~0 << (BITS-1);
	max = ~(~0 << (BITS-1));
	umax = ~0;
	printf("bitshift: char\n");
	printf(" %19d\tmin\n%20d\tmax\n%20u\tumax\n", min, max, umax);
	puts("");
	return;
} 

/* range_short: print short type value ranges */
void range_short(void)
{
	unsigned short max, umax;
	short min;
	int BITS = 16;

	min = SHRT_MIN;	
	max = SHRT_MAX;
	umax = USHRT_MAX;
	printf("limit.h: short\n");
	printf(" %19d\tmin\n%20d\tmax\n%20u\tumax\n", min, max, umax);
	
	min = ~0 << (BITS-1);
	max = ~(~0 << (BITS-1));
	umax = ~0;
	printf("bitshift: short\n");
	printf(" %19d\tmin\n%20d\tmax\n%20u\tumax\n", min, max, umax);
	puts("");
	return;
} 


/* range_int: print int type value ranges */
void range_int(void)
{
	unsigned int max, umax;
	int min;
	int BITS = 32;

	min = INT_MIN;	
	max = INT_MAX;
	umax = UINT_MAX;
	printf("limit.h: int\n");
	printf(" %19d\tmin\n%20d\tmax\n%20u\tumax\n", min, max, umax);
	
	min = ~0 << (BITS-1);
	max = ~(~0 << (BITS-1));
	umax = ~0;
	printf("bitshift: int\n");
	printf(" %19d\tmin\n%20d\tmax\n%20u\tumax\n", min, max, umax);
	puts("");
	return;
} 

/* range_long: print long type value ranges */
void range_long(void)
{
	unsigned long max, umax;
	long min;
	int BITS = 64;

	min = LONG_MIN;	
	max = LONG_MAX;
	umax = ULONG_MAX;
	printf("limit.h: long\n");
	printf("%19ld\tmin\n%20ld\tmax\n%20lu\tumax\n", min, max, umax);	

	min = ~0 << (BITS-1);
	max = ~(~0 << (BITS-1)); 
	umax = ~0;
	printf("bitshift: long\n");
	printf(" %19ld\tmin\n%20ld\tmax\n%20lu\tumax\n", min, max, umax);	

	return;
} 
