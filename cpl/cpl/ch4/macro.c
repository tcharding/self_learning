#include <stdio.h>

/* Macro swap(t, x, y): interchange two arguments of type t */
#define swap(T, X, Y) { T tmp; tmp = X; X = Y; Y = tmp; }    

/* ## concatenates its two arguments */
#define paste(front, back) front ## back 

/* #var expands into "var" */
#define dprint(expr) printf(#expr " = %d\n", expr)

int main(void)
{
	int x = 1;
	int y = 2;

	printf("Initial values\n");
	printf("x: %d\ty: %d\n", x, y);
	swap(int , x, y);
	printf("After swap macro\n");
	printf("x: %d\ty: %d\n", x, y);

	dprint(x + y);
	return 0;
}

