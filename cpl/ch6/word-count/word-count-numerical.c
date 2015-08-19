#include <stdio.h>
#include <ctype.h>
#include <string.h>
#include "tnode.h"
#include "adt.h"

#define MAXWORD 100

/* prototypes */
static void insert(char *, int);	/* insert values into adt */

struct adt *flat;	/* used to flatten tree */

/* word frequency count */
int main(void)
{
	struct tnode *root;
	char word[MAXWORD];
	struct word *w;
	int getword(char *, int);
	
	root = NULL;
	while (getword(word, MAXWORD) != EOF)
		if (isalpha(word[0]))
			root = addtree(root, word);
	
	flat = adt_creat();
	treedump(root, &insert);
	adt_sort(flat);
	adt_print(flat);
	return 0;
}

/* insert: copy w and cnt into global adt */
static void insert(char *w, int cnt)
{
	adt_add(flat, w, cnt);
}
