#include <string.h>
#include <ctype.h>
#include <stdio.h>

enum { MAXLINE = 1024 };

double atof(char s[]);
int getline(char s[], int lim);
double exp_multiplier(int val, int sign);

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
}
/* atof: convert string s to double */
double atof(char s[])
{
	double val, power;
	int i, sign;
	int x, xsign;	/* scientific notation e.g 3.12e-4 */

	x = 0;
	xsign = 1;
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
	
	if (s[i] == 'e' || s[i] == 'E') { /* handle scientific */
		i++;
		xsign = (s[i] == '-') ? -1 : 1;
		if (s[i] == '+' || s[i] == '-')
			i++;
		for (x = 0; isdigit(s[i]); i++)
			x = 10 * x + (s[i] - '0');
	}
	return sign * val / power * exp_multiplier(x, xsign);

}

/* getline: get line into s, return length */
int getline(char s[], int lim)
{
	int c, i;

	i = 0;
	while (--lim > 0 && (c = getchar()) != EOF && c != '\n')
		s[i++] = c;
	if (c == '\n')
		s[i++] = c;
	s[i] = '\0';
	return i;
}

/* exp_multiplier: return multiplier. e.g for 3.12e-4 return 0.0001 */
double exp_multiplier(int val, int sign)
{
	int i;
	double mult = 10.0;

	if (val == 0)		/* x to power of 0 = 1 */
		return 1.0;

	if (sign < 0) 
		for (i = 0; i <= val; i++) 
			mult /= 10.0;
	else
		for (i = 0; i < val-1; i++) 
			mult *= 10.0;
	return mult;
}
