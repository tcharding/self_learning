#include <stdio.h>
#include <stdlib.h>

/* 
 * Reverse Polish Calculator
 *
 * Based on code in Kernighan and Ritchie, second edition 
 */

enum {
	NUMBER = '0',		/* signal that a number was found */
	NOOP = 300,		/* signal that op is invalid */
	SIN = 500,		/* math function signals 5** */
	COS = 501,
	TAN = 502,
	POW = 503,
	ANS = 600,		/* variable signal */
};

/* getop.c */
int getop(char buf[]);

/* getch.c */
int getch(void);
void ungetch(int);

/* math function prototypes, math.c */
double unary(int type, double val);
double binary(int type, double op1, double op2);

/* stack manipulation prototypes, stack.c */
void push(double val);
double pop(void);
void peek();
void dup_top();
void swap();
void clear();

