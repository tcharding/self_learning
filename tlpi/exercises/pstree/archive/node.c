#include "tlpi_hdr.h"
#include "pstree.h"

/* print a complete process tree (includes all running processes) */

/* newProc: allocate memory and create new proc node */
struct proc *newProc(pid_t ppid, pid_t pid, const char *cmd)
{
	struct proc *p;

	p = malloc(sizeof(struct proc));
	if (p == NULL)
		errExit("malloc");

	p->ppid = ppid;
	p->pid = pid;
	p->cmd = strdup(cmd);
	p->chlds = NULL;

	return p;
}

/* freeProc: free proc created with newProc */
void
freeProc(struct proc *p)
{
	if (p != NULL) {
		if (p->cmd != NULL)
			free(p->cmd);
		if(p->chlds != NULL) {
			struct procL *ptr, *tmp;
			ptr = ptr->childs;
			while (ptr != NULL) {
				tmp = ptr;
				ptr = ptr->next;
				freeProc(tmp->p);
				free(tmp);
			}
		}
		free(p);
	}
}

/* add: add proc to tree */
struct proc *
add(struct proc *root, struct proc *new)
{
	struct proc *p;
	struct procL *pl;
	
	p = findProc(root, new->ppid);
	if (p == NULL)
		return NULL;

	if (p->procL == NULL) {	/* first child */
		p->procL = newProcL
	}
	while (pl != NULL)
		pl = pl->next;
}

/* printProc: pretty print tree */
void
printProcTree(struct proc *root)
{
	
}


