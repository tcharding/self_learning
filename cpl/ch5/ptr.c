#include <stdio.h>
#include <string.h>

enum { MAXLINE = 4096 };

void copy(char *to, char *from); 
void reverse(char *line);
int th_getline(char *s, int maxline);
void itoa(int n, char *s);

/* reverse input line by line  */
int main(void)
{
	char line[MAXLINE];
	int len;
	int n;

	/*
	  test itoa 
	n = -31320;
	itoa(n, line);
	printf("%s\n", line);
	return 0;
	*/

	/* test th_getline
	while ( (len = th_getline(line, MAXLINE)) != EOF) {
		if (len != 0) {
			if (line[len-1] == '\n')
				line[len-1] = '\0';
			reverse(line);
		}
		printf("%s", line);
	}
	return 0;
	*/
}

/* reverse: reverse s in place */
void reverse(char *s)
{
	char *t = s;
	int tmp;

	t += strlen(s) - 1;
	for ( ; s < t; ++s, --t) 
		tmp = *s, *s = *t, *t = tmp;
}
			
			
/* copy: copy 'from' into 'to'; assume to is big enough */
void copy(char to[], char from[])
{
	int i;

	i = 0;
	while ( (to[i] = from[i]) != '\0')
		++i;
}

/* th_getline: read a line into s, strip trailing whitespace, return length */
int th_getline(char *s, int lim)
{
	int c, i;

	for (i = 0; i < lim-1 && (c = getchar()) != EOF && c != '\n'; i++)
		*(s+i) = c;

	if (c == EOF && i == 0)
		return EOF;

	if (c == '\n') {
		*(s+i) = c;
		++i;
	}

	*(s+i) = '\0';
	return i;
}


/* itoa: convert n to characters in s */
void itoa(int n, char *s)
{
	int i, sign;
	unsigned u;
	char *save = s;
	
	sign = n;		     /* record sign */
	u = (n < 0) ? n * -1 : n;    /* make it positive */
	do {			     /* generate digits in reverse order */
		*s++ = u % 10 + '0'; /* get next digit */
	} while ( (u /= 10) > 0);    /* delete it */
	if (sign < 0) 
		*s++ = '-';
	*s = '\0';
	reverse(save); 
}

