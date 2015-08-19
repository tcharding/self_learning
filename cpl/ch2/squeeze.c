#include <stdio.h>

enum { FALSE = 0, TRUE };

yint contains(char *s, char c);

/* squeeze: remove from s any characters in t */
void squeeze(char *s, char *t)
{
	int i, j;

	for (i = 0; s[i] != '\0'; i++)
		if (!contains(t, s[i]))
			s[j++] = s[i];
	s[i] = '\0';
}

/* contains: true if s contains c */
int contains(char *s, char c)
{
	int i;

	for (i = 0; s[i] != '\0'; i++)
		if (s[i] == c)
			return TRUE; 
	return FALSE;		 
}
