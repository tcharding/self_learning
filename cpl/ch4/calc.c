#include "calc.h"

/* 
 * Reverse Polish Calculator
 *
 * Based on code in Kernighan and Ritchie, second edition 
 */

enum {
	MAXOP = 100,		/* max size of operand or operator */
};


int main(void)
{
	int type;
	double op2;
	char s[MAXOP];
	static double a = 0.0;	/* ANSWER variable */
	
	while ( (type = getop(s)) != EOF) {
		switch (type) {
		case NUMBER:
			push(atof(s));
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
		case SIN: case COS: case TAN: /* fall through, all unary */
			push(unary(type, pop()));
			break;
		case POW:
			op2 = pop();
			push(binary(type, pop(), op2));
			break;
		case ANS:
			push(a);
			break;
		case '\n':
			a = pop();
			printf("\t%.8g\n", a);
			break;
		case -1:	/* fall through to default error */
		default:
			printf("error: unknown command %s\n", s);
			break;
		}
	}
	return 0;
}
