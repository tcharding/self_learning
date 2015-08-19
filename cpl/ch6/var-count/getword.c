#include <stdio.h>
#include <ctype.h>

/* getword: get next word or character from input, return NULL on EOF */
char *getword(char *word, int lim)
{
	int c, d, getch(void);
	void ungetch(int);
	char *w = word;

	while (isspace(c = getch()))
		;
	*w++ = c;
	if (c == EOF) {		/* end of file */
		*w = '\0';
		return NULL;
	}
	
	if (c == '\"') {		/* handle double quotes */
		while ((*w++ = getch()) != '\"')
			;
		/*
			if (*w == EOF) {
				fprintf(stderr, "syntax error: mismatched quotes\n");
				break;
			}
		*/
		*w = '\0';
		return word;
	}
	if (c == '\'') {		/* handle single quotes */
		while ((*w++ = getch()) != '\'')
			if (*w == EOF) {
				fprintf(stderr, "syntax error: mismatched quotes\n");
				break;
			}
		*w = '\0';
		return word;
	}
	if (c == '/') {		/* handle start of comment */
		if ((d = getch()) == '*')
			*w++ = d;
		else
			ungetch(d);
		*w = '\0';
		return word;
	}
	if (c == '*') {	/* handle end of comment */
/*		((d = getch()) == '/') ? *w++ = d : ungetch(d); */
		if ((d = getch()) == '/')
			*w++ = d;
		else
			ungetch(d);
		*w = '\0';
		return word;
	}
	for ( ; --lim > 0; w++) {
		if (!(isalnum(c = getchar()))) {
			ungetch(c);
			break;
		}
		*w = c;
	}
	*w = '\0';
	return word;
       
}
