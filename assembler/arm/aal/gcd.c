#include <stdio.h>

int gcd(int a, int b)
{
	while (a != b) {
		if (a > b)
			a = a - b;
		else
			b = b - a;
	}

	return a;		/* or b */
}

int main(void)
{
	int a, b, res;

	a = 3;
	b = 15;

	res = gcd(a, b);

	printf("gcd of (%d, %d): %d\n", a, b, res);

	return 0;
}


