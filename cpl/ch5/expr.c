#include <stdlib.h>
#include <stdio.h>

/* stack manipulation prototypes, stack.c */
void push(double val);
double pop(void);
void peek();
void dup_top();
void swap();
void clear();

/* evaluate reverse polish expression */
int main(int argc, char *argv[])
{
	double op2;
	int c;
	
	while (--argc > 0) {
		++argv;
		c = **argv;
		switch (c) {
		case '0': case '1': case '2': case '3': case '4':
		case '5': case '6': case '7': case '8': case '9': /* fall through */
			push(atof(*argv));
			break;
		case '+':
			push(pop() + pop());
			break;
		case '*':
			push(pop() * pop());
			break;
		case '-':
			op2 = pop();
			push(pop() - op2);
			break;
		case '/':
			op2 = pop();
			if (op2 != 0.0)
				push(pop() / op2);
			else
				printf("error: zero divisor\n");
			break;
		case '%':
			op2 = pop();
			if (op2 != 0)
				push((int)pop() % (int)op2);
			else
				printf("error: zero divisor\n");
			break;
		default:
			printf("error: unknown operand\n");
			return 1;
		}
	}
	printf("\t%.8g\n", pop());
		
	return 0;
}
       
