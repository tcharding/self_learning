/* Exercise 12.2 */
#include <pwd.h>
#include <dirent.h>
#include <ctype.h>
#include <sys/stat.h>
#include <fcntl.h>
#include "tlpi_hdr.h"
#include "pstree.h"

/* print a complete process tree (includes all running processes) */


static struct node *testTree(void);


int main(void)
{
	struct node *root;

	root = testTree();
	printTree(root);
	/* freeNode(root); */

	exit(EXIT_SUCCESS);
}

/* /\* buidProcList: scan /proc and build list *\/ */
/* void buildProcList(procList_t **ptr) */
/* { */
	
/* } */

/* /\* buildTree: build process tree from list *\/ */
/* struct proc *buildTree(procList_t *pl) */
/* { */
	
/* } */

struct node *
testTree(void)
{
	struct node *root;

	root = node(0, 1, "init");

	if (add(root, node((pid_t) 1, (pid_t) 11, "win")))
		errExit("add");
	if (add(root, node((pid_t) 1, (pid_t) 12, "loose")))
		errExit("add");
	if (add(root, node((pid_t) 1, (pid_t) 13, "draw")))
		errExit("add");

	if (add(root, node((pid_t) 11, (pid_t) 111, "profit")))
		errExit("add");
	if (add(root, node((pid_t) 11, (pid_t) 112, "zsh")))
		errExit("add");
	if (add(root, node((pid_t) 11, (pid_t) 113, "zsh")))
		errExit("add");

	if (add(root, node((pid_t) 13, (pid_t) 131, "zsh")))
		errExit("add");
	if (add(root, node((pid_t) 13, (pid_t) 132, "zsh")))
		errExit("add");

	return root;
}

