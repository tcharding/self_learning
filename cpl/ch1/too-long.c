#include <stdio.h>

enum { MAXLINE = 128, LIMIT = 80 };
int mygetline(char line[], int maxline);
void copy(char to[], char from[]);

/* prints longest input line  */
int main(void)
{
	int len;		/* current line length */
	char line[MAXLINE];	/* current input line */

	while ( (len = mygetline(line, MAXLINE)) > 0)
		if (len > LIMIT)
			printf("%s", line);

	return 0;
}

/* mygetline: read a line into s, return length */
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

