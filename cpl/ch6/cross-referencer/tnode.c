#include "tnode.h"
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#define _XOPEN_SOURCE 700

static struct tnode *talloc(void);
char *strdup(const char *s);

/* addtree: add a node with w, at or below p */
struct tnode *addtree(struct tnode *p, char *w, int lineno)
{
	int cond;

	if (p == NULL) {	/* a new word has arrived */
		p = talloc();	/* make a new node */
		p->word = strdup(w);
		p->ref = dia_creat();
		dia_add(p->ref, lineno);
		p->left = p->right = NULL;
	} else if ((cond = strcmp(w, p->word)) == 0)
		dia_add(p->ref, lineno); /* repeated word */
	else if (cond < 0)	/* less than into left subtree */
		p->left = addtree(p->left, w, lineno);
	else			/* greater than into right subtree */
		p->right = addtree(p->right, w, lineno);
	return p;
}

/* treeprint: in-order print of tree p */
void treeprint(struct tnode *p)
{
	if (p != NULL) {
		treeprint(p->left);
		printf("%s (", p->word);
		dia_print(p->ref);
		printf(")\n");
		treeprint(p->right);
	}
}

/* talloc: make a tnode */
static struct tnode *talloc(void)
{
	return (struct tnode *)malloc(sizeof(struct tnode));
}
