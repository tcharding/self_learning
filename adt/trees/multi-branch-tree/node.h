#ifndef NODE_H
#define NODE_H

struct node {
	int id;
	struct node *child;
	struct node *next;
};

struct node *node(int id);
void freeNode(struct node *ptr);
int addTreeParentIdChild(struct node *root, int parentId, struct node *child);
void printTree(struct node *root);

#endif	/* NODE_H */
