#include "tnode.h"
#include <stdio.h>
#include <ctype.h>
#include <string.h>

#define MAXWORD 100
static int getword(char *, int);
static int noise(const char *w);

int lineno = ;1
char *noisetab[] = {
	"this", "is", "the", "but", "and",
	"This", "Is", "The", "But", "And",
	NULL
};

/* word frequency count */
int main(void)
{
	struct tnode *root;
	char word[MAXWORD];

	root = NULL;
	while (getword(word, MAXWORD) != EOF)
		if (isalnum(word[0]) && !noise(word))
			root = addtree(root, word, lineno);
	treeprint(root);
	return 0;
}

/* getword: get next word or character from input */
int getword(char *word, int lim)
{
	int c, getch(void);
	void ungetch(int);
	char *w = word;

	while (isspace(c = getch()))
		if (c == '\n')
			lineno++;
	if (c != EOF)
		*w++ = c;
	if (!isalnum(c)) {
		*w = '\0';
		return c;
	}
	for ( ; --lim > 0; w++) 
		if (!isalnum(*w = getch())) {
			ungetch(*w);
			break;
		}
	*w = '\0';
	return word[0];
}
/* noise: return non-zero if w is classified as noise */
static int noise(const char *w)
{
	char **np = noisetab;
	while (*np != NULL) {
		if (strcmp(*np, w) == 0)
		    return 1;
		++np;
	}
	return 0;
}
