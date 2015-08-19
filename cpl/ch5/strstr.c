#include <stdio.h>

enum { MAXLINE = 1024 };

int th_getline(char line[], int max);
int strindex(char source[], char searchfor[]);
char *th_strstr(char *haystack, char *needle);

char pattern[] = "ould";	/* pattern to search for */

/* find all lines matching pattern */
int main(void)
{
	char *haystack = "this is a string";
	char *sub;

	if ( (sub = th_strstr(haystack, "not there")) != NULL)
		fprintf(stderr, "Failed negative strstr");
	if  ( (sub = th_strstr(haystack, "string")) == NULL)
		fprintf(stderr, "Failed positive strstr");
	fprintf(stderr, "got substring: %s\n", sub);
	return 0;
}
/* th_getline: read a line into s, strip trailing whitespace, return length */
int th_getline(char *s, int lim)
{
	int c, i;

	for (i = 0; i < lim-1 && (c = getchar()) != EOF && c != '\n'; i++, s++)
		*s = c;

	if (c == EOF && i == 0)
		return EOF;

	if (c == '\n') {
		*s++ = c;
		i++;
	}
	*s = '\0';
	return i;
}

/* th_strstr: find the first occurrence of substring needle in haystack 
     terminating '\0' are not compared, return substring or NULL if not found */
char *th_strstr(char *haystack, char *needle)
{
	char *h, *n;

	while (*haystack) {
		h = haystack;
		n = needle;
		while (*n == *h && *h && *n)
			++h, ++n;
		if (!*h)
			return haystack;
		++haystack;
	}
	return NULL;
}

int strindex(char *s, char  *t)
{
	int i, j, k;

	for (i = 0; s[i] != '\0'; i++) {
		for (j = i, k = 0; t[k] != '\0' && s[j] == t[k]; j++, k++)
			;
		if (k > 0 && t[k] == '\0')
			return i;
	}
	return -1;
}
