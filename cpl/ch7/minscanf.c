#include <ctype.h>
#include <stdio.h>
#include <stdarg.h>
#define MAX 1024

int minscanf(char *fmt, ...);

/* test minscanf */
int main(int argc, char *argv[])
{
	char s[MAX];
	int i;
	double d;	

	scanf("%s %d %lf\n", s, &i, &d);
	printf("scanf:\n\t%s\n\t%d\n\t%f\n", s, i, d);
	minscanf("%s %d %f\n", s, &i, &d);
	printf("minscanf:\n\t%s\n\t%d\n\t%f", s, i, d);
	return 0;
}

/* minscanf: minimal scanf with variable argument list */
int minscanf(char *fmt, ...)
{
	va_list ap;		/* points to each unnamed arg in turn */
	char *p, *sval;
	int ival;
	double dval;
	unsigned int uval;
	int cnt, c, getch();
	
	cnt = 0;
	va_start(ap, fmt);	/* make ap point to first unnamed arg */
	for (p = fmt; *p; p++) {
		if (*p != '%') {
			while (isspace(c = getch()) && c != EOF)
				;
			if (c == EOF)
				return EOF;
			if (c != *p)
				return 0; /* incorrect input string */
			continue;
		}
		switch(*++p) {
		case 'd':
			ival = va_arg(ap, int);
			scanf("%d", &ival);
			cnt++;
			break;
		case 'f':
			dval = va_arg(ap, double);
			scanf("%lf", &dval);
			cnt++;
			break;
		case 's':
			sval = va_arg(ap, char *);
			scanf("%s", sval);
			cnt++;
			break;
		case 'x':
			uval = va_arg(ap, int);
			scanf("%x", &uval);
			cnt++;
			break;
		}
	}
	va_end(ap);		/* clean up when done */
	return cnt;
}
