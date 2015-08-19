#include <stdio.h>

enum { MAXLINE = 8 };

int mygetline(char line[], int maxline);
void copy(char to[], char from[]);

/* prints longest input line  */
int main(void)
{
	int len;		/* current line length */
	int max;		/* maximum length soo so far */
	char line[MAXLINE];	/* current input line */
	char longest[MAXLINE];

	max = 0;
	while ( (len = mygetline(line, MAXLINE)) > 0)
		if (len > max) {
			max = len;
			copy(longest, line);
		}
	if (max > 0)		/* there was a line */
		printf("%d: %s", max, longest);
	return 0;
}

/* mygetline:
 read a line into s, return length */
int mygetline(char s[], int lim)
{
	int c, i;

	for (i = 0; i < lim-1 && (c = getchar()) != EOF && c != '\n'; ++i)
		s[i] = c;
	if (i == lim-1) {	
		for ( ; ((c = getchar()) != EOF) && c != '\n'; ++i)
			;	/* loop over remaining characters */
		s[lim-1] = '\0';
		return i;
	}
	if (c == '\n') {
		s[i] = c;
		++i;
	}
	s[i] = '\0';
	return i;
}

/* copy: copy 'from' into 'to'; assume to is big enough */
void copy(char to[], char from[])
{
	int i;

	i = 0;
	while ( (to[i] = from[i]) != '\0')
		++i;
}
