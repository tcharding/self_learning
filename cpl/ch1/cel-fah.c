#include <stdio.h>

/* print Celsius-Fahrenheit table for celsius = -40, -20, ..., 100 */
int main(void)
{
	int fahr, celsius;
	int lower, upper, step;

	lower = -40;		/* lower limit of tempreature table */
	upper = 100;		/* upper limit */
	step = 20;		/* step size */

	celsius = lower;
	printf("Celsius\t\tFarhenheit\n");
	puts("------------------------");
	puts("");
	while (celsius <= upper) {
		fahr = celsius * 9 / 5 + 32;
		printf("%d\t\t%d\n", celsius, fahr);
		celsius += step;
	}
}
