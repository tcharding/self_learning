#include "calc.h"
#include <math.h>

/* unary: return result of unary math function */
double unary(int type, double val)
{
	switch (type) {
	case SIN:
		return sin(val);
		break;
	case COS:
		return cos(val);
		break;
	case TAN:
		return tan(val);
		break;
	default:
		fprintf(stderr, "error: unary function not supported\n");
		break;
	}
	return 0.0;		/* error */
}

/* binary: return result of binary math function */
double binary(int type, double op1, double op2)
{
	switch (type) {
	case POW:
		return pow(op1, op2);
		break;
	default:
		fprintf(stderr, "error: binary function not supported\n");
		break;
	}
	return 0.0;		/* error */
}

