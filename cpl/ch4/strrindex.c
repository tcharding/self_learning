#include <stdio.h>
#include <string.h>

enum { MAXLINE = 1024 };

int mygetline(char line[], int max);
int strrindex(char source[], char searchfor[]);

char pattern[] = "ould";	/* pattern to search for */

/* find all lines matching pattern */
int main(void)
{
	char line[MAXLINE];
	int found = 0;
	int index;
	
	while (mygetline(line, MAXLINE) > 0) 
		if ( ( index = strrindex(line, pattern)) >= 0) {
			printf("%s : %d\n", line, index);
			found++;
		}
	return found;
}

/* mygetline: get line into s, return length */
int mygetline(char s[], int lim)
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

/* strrindex: return index of rightmost t in s, -1 if none */
int strrindex(char s[], char t[])
{
	int i, j, k;

	for (i = strlen(s) - 1; i >= 0; i--) {
		for (j = i, k = 0; t[k] != '\0' && s[j] == t[k]; j++, k++)
			;
		if (k > 0 && t[k] == '\0')
			return i;
	}
	return -1;
}
