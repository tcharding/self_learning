#include <stdio.h>
#include <ctype.h>

/* This version does not handle string constants */				

/* getword: get next word or character from input */
int getword(char *word, int lim)
{
	int c, getch(void);
	void ungetch(int);
	char *w = word;

	while (isspace(c = getch()))
		;
	if (c != EOF)
		*w++ = c;
	if (!(c == '#' || c == '_' || isalpha(c))) {
		*w = '\0';
		return c;
	}
	for ( ; --lim > 0; w++) {
		if (!(((c = getch()) == '_') || isalnum(c))) {
			ungetch(c);
			break;
		}
		*w = c;
	}
	*w = '\0';
	return word[0];
}

