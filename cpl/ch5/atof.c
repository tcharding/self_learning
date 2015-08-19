#include "sort.h"
#include <ctype.h>

/*
enum {
	MAXLINE = 4096
};

int getline(char *s, int lim);
double atof(char s[]);

int main(void)
{
	int len;
	double val;
	char buf[MAXLINE];
	
	while (getline(buf, MAXLINE) > 0) {
		len = strlen(buf);
		if (buf[len-1] == '\n')
		    buf[len-1] = '\0';
		val = atof(buf);
		printf("atof(%s): %f\n", buf, val);
	}
	return 0;
}
*/
/* atof: convert string s to double */
double atof(const char s[])
{
	double val, power;
	int i, sign;

	for (i = 0; isspace(s[i]); i++) /* skip whitespace */
		;
	sign = (s[i] == '-') ? -1 : 1;
	if (s[i] == '+' || s[i] == '-')
		i++;
	for (val = 0.0; isdigit(s[i]); i++)
		val = 10.0 * val + (s[i] - '0');
	if (s[i] == '.') 	/* handle fraction */
		i++;
	for (power = 1.0; isdigit(s[i]); i++) {
		val = 10.0 * val + (s[i] - '0');
		power *= 10.0;
	}
	
	return sign * val / power;
}
