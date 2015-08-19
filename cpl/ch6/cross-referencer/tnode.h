#ifndef _TNODE_H
#define _TNODE_H
#include "dia.h"

struct tnode {			/* tree node */
	char *word;
	struct dia *ref;
	struct tnode *left;
	struct tnode *right;
};

/* prototypes */
struct tnode *addtree(struct tnode *p, char *word, int lineno);
void treeprint(struct tnode *);

#endif	/*  _TNODE_H */
