#include "tnode.h"
#include "adt.h"
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#define _XOPEN_SOURCE 700

static struct tnode *talloc(void);
char *strdup(const char *s);

/* addtree: add a node with w, at or below p */
struct tnode *addtree(struct tnode *p, char *w)
{
	int cond;

	if (p == NULL) {	/* a new word has arrived */
		p = talloc();	/* make a new node */
		p->word = strdup(w);
		p->count = 1;
		p->left = p->right = NULL;
	} else if ((cond = strcmp(w, p->word)) == 0)
		p->count++;	/* repeated word */
	else if (cond < 0)	/* less than into left subtree */
		p->left = addtree(p->left, w);
	else			/* greater than into right subtree */
		p->right = addtree(p->right, w);
	return p;
}

/* treeprint: in-order print of tree p */
void treeprint(struct tnode *p)
{
	if (p != NULL) {
		treeprint(p->left);
		printf("%4d %s\n", p->count, p->word);
		treeprint(p->right);
	}
}

/* treedump: copy tree values to ADT */
void treedump(struct tnode *p, void (*dump)(char *, int))
{
	if (p != NULL) {
		treedump(p->left, dump);
		(*dump)(p->word, p->count);
		treedump(p->right, dump);
	}
}

/* talloc: make a tnode */
static struct tnode *talloc(void)
{
	return (struct tnode *)malloc(sizeof(struct tnode));
}
