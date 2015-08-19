#ifndef _TNODE_H
#define _TNODE_H

struct tnode {			/* tree node */
	char *word;
	int count;
	struct tnode *left;
	struct tnode *right;
};

/* prototypes */
struct tnode *addtree(struct tnode *, char *); /* add word to tree */
void treeprint(struct tnode *);		       /* print tree to stdout */
void treedump(struct tnode *, void (*)(char *, int)); /* copy tree into ADT */

#endif	/*  _TNODE_H */
