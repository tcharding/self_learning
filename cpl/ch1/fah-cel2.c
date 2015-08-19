#include "th.h"

enum {
	LOWER = 0,
	UPPER = 300,
	STEP = 20
};

/* print Fahrenheit-Celsius table for fahr = 0, 20, ..., 300 ; 2nd version */
int main(void)
{
 	int fahr;

	printf("  F     C\n");
	puts("------------");

	for (fahr = UPPER; fahr >= LOWER; fahr = fahr - STEP)
		printf("%3d %6.1f\n", fahr, (5.0/9.0)*(fahr-32));

	exit(0);
}
