/* Exercise 12.2 */
#include <pwd.h>
#include <dirent.h>
#include <ctype.h>
#include <sys/stat.h>
#include <fcntl.h>
#include "tlpi_hdr.h"
#include "pstree.h"

/* print a complete process tree (includes all running processes) */

/*
static void buildProcList(procList_t **ptr);
static struct proc *buildTree(procList_t *pl);
*/
static struct proc *testTree(void);


int main(void)
{
	struct proc *root;

	root = testTree();
	printProcTree(root);

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

struct proc *
testTree(void)
{
	struct proc *root;

	root = newProc(0, 1, "init");
	
	root = add(root, newProc((pid_t) 1, (pid_t) 2, "win"));
	root = add(root, newProc((pid_t) 1, (pid_t) 3, "loose"));
	root = add(root, newProc((pid_t) 1, (pid_t) 4, "draw"));
	
	root = add(root, newProc((pid_t) 2, (pid_t) 11, "profit"));
	root = add(root, newProc((pid_t) 2, (pid_t) 12, "zsh"));
	root = add(root, newProc((pid_t) 2, (pid_t) 13, "zsh"));

	root = add(root, newProc((pid_t) 4, (pid_t) 14, "zsh"));
	root = add(root, newProc((pid_t) 4, (pid_t) 15, "zsh"));

	return root;
}

