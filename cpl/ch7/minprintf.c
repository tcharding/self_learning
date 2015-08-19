#include <stdio.h>
#include <stdarg.h>

void minprintf(char *fmt, ...);

/* test minprintf */
int main(int argc, char *argv[])
{

	minprintf("%s\n", "this stirng");
	minprintf("%d\n", 10);
	minprintf("%f\n", 1.157);
	minprintf("%x\n



", 15);
	return 0;
}

/* minprintf: minimal printf with variable argument list */
void minprintf(char *fmt, ...)
{
	va_list ap;		/* points to each unnamed arg in turn */
	char *p, *sval;
	int ival;
	double dval;

	va_start(ap, fmt);	/* make ap point to first unnamed arg */
	for (p = fmt; *p; p++) {
		if (*p != '%') {
			putchar(*p);
			continue;
		}
		switch(*++p) {
		case 'd':
			ival = va_arg(ap, int);
			printf("%d", ival);
			break;
		case 'f':
			dval = va_arg(ap, double);
			printf("%f", dval);
			break;
		case 's':
			for (sval = va_arg(ap, char *); *sval; sval++)
				putchar(*sval);
			break;
		case 'x':
			ival = va_arg(ap, int);
			printf("%x", ival);
			break;
		}
	}
	va_end(ap);		/* clean up when done */
}
