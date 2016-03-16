#ifndef PSTREE_H
#define PSTREE_H

#include <unistd.h>

struct proc {
	pid_t ppid;
	pid_t pid;
	char *cmd;
};

struct node {
	struct proc *p;
	struct node *child;
	struct node *next;
};

struct node *node(pid_t ppid, pid_t pid, const char *cmd);
void freeNode(struct node *ptr);
int add(struct node *root, struct node *child);
void printTree(struct node *root);

#endif	/* PSTREE_H */
