#include <stdio.h>

/* while (get line) */
/* 	if line ends in blanks of tabs remove them */
/* 	if blank line skip it */
/* 	output line */
enum { MAXLINE = 4096 };

int mygetline(char line[], int maxline);

int main(void)
{
	char line[MAXLINE];
	int len;
	
	while ( (len = mygetline(line, MAXLINE)) != EOF) {
		if (len == 0)
			continue; /* skip blank lines */
		printf("%s[END]\n", line);
	}
	return 0;
}

/* mygetline: read a line into s, strip trailing whitespace, return length */
int mygetline(char s[], int lim)
{
	int c, i;

	for (i = 0; i < lim-1 && (c = getchar()) != EOF && c != '\n'; ++i)
		s[i] = c;
	if (c == '\n' && i == 0)		/* blank line */
		return 0;
	if (c == EOF && i == 0)
		return EOF;
		
	for ( ; s[i-1] == ' ' || s[i-1] == '\t'; i--)
		;
/*
	if (c == '\n') {
		s[i] = c;
		++i;
	}
*/
	s[i] = '\0';
	return i;
}
	
