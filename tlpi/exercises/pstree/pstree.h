#ifndef PSTREE_H
#define PSTREE_H

#include "tlpi_hdr.h"

struct node {
	struct proc *p;
	struct node *child;
	struct node *next;
};

struct node *node(pid_t ppid, pid_t pid, const char *cmd);
int addChild(struct node **root, struct node *child);
int addProc(struct node **root, pid_t ppid, pid_t pid, const char *cmd);
void freeTree(struct node *root);
void printTree(struct node *root);
void printSiblings(struct node *n);
Boolean canAdd(struct node *root, struct node *child);
size_t treeSize(struct node *root);

void testNode(void);
struct node *buildTestTree(void);

#endif	/* PSTREE_H */
