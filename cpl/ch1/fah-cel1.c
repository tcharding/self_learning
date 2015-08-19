#include <stdio.h>
#include <stdlib.h>

/* print Fahrenheit-Celsius table for fahr = 0, 20, ..., 300 ; 1st version */
int main(void)
{
	int fahr, celsius;
	int lower, upper, step;

	lower = 0;		/* lower limit of tempreature table */
	upper = 300;		/* upper limit */
	step = 20;		/* step size */

	fahr = lower;
	printf("Farhenheit\tCelsius\n");
	puts("------------------------");
	puts("");
	while (fahr <= upper) {
		celsius = 5 * (fahr-32) / 9;
		printf("%d\t\t%d\n", fahr, celsius);
		fahr = fahr + step;
	}

	exit(0);
}

