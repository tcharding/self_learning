#ifndef PSTREE_H
#define PSTREE_H

/* process node */
struct proc {
	pid_t ppid;
	pid_t pid;
	char *cmd;
	struct procL *chlds;
};

/* process List node */
struct procL {
	struct proc *p;
	struct procL *next;
};

				/* node.c */
struct proc *newProc(int ppid, int pid, const char *cmd);
void freeProc(struct proc *p);
struct proc *add(struct proc *root, struct proc *proc);
void printProcTree(struct proc *root);

#endif	/* PSTREE_H */
