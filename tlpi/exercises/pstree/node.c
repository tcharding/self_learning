#include "pstree.h"
#include "tlpi_hdr.h"

static struct proc *newProc(pid_t ppid, pid_t pid, const char *cmd);
static void freeProc(struct proc *p);
static struct node *getNode(struct node *root, pid_t pid);
static void printTreeR(struct node *node, int level);

static void freeProc(struct proc *p);
static struct node *getNodeR(struct node *node, pid_t pid);
static void prettyPrintNode(struct node *node, int level);

/* node: allocate memory and create node */
struct node *
node(pid_t ppid, pid_t pid, const char *cmd)
{
	struct node *node;

	node = malloc(sizeof(struct node));
	if (node == NULL) {
		errno = ENOMEM;
		return NULL;
	}
	node->p = newProc(ppid, pid, cmd);
	node->child = NULL;
	node->next = NULL;

	return node;
}

/* freeNode: free node created with node() */
void freeNode(struct node *node)
{
	if (node == NULL)
		return;

	freeNode(node->child);
	freeNode(node->next);
	freeProc(node->p);
}

/* addTreeParentIdChild: add child node to parentId starting at root
    return non-zero if parent not found */
int
add(struct node *root, struct node *child)
{
	struct node *parent, *ptr;

	parent = getNode(root, child->p->ppid);
	if (parent == NULL)
		return 1;

	if (parent->child == NULL) { /* first child */
		parent->child = child;
	} else {		/* add to end of child list */
		ptr = parent->child;
		while (ptr->next != NULL)
			ptr = ptr->next;
		ptr->next = child;
	}

	return 0;
}

/* printTree: pretty print tree */
void printTree(struct node *root)
{
	printTreeR(root, 0);
}

/* newProc: allocate memory and create proc */
static struct proc *
newProc(pid_t ppid, pid_t pid, const char *cmd)
{
	struct proc *p;

	p = malloc(sizeof(struct proc));
	if (p == NULL)
		return NULL;

	p->ppid = ppid;
	p->pid = pid;
	p->cmd = strdup(cmd);

	return p;
}

/* freeProc: helper function called by freeNode. Don't call this directly */
static void
freeProc(struct proc *p)
{
	if (p != NULL) {
		if (p->cmd != NULL)
			free(p->cmd);
		free(p);
	}
}

/* getNode: return node with id, NULL if node not found */
static struct node *
getNode(struct node *root, pid_t pid)
{
	return getNodeR(root, pid);
}

/* printTreeR: recursive helper */
static void
printTreeR(struct node *node, int level)
{
	if (node == NULL)
		return;

	prettyPrintNode(node, level);
	printTreeR(node->child, level+1);
	printTreeR(node->next, level);
}

/* getNodeR: recursive helper */
static struct node *
getNodeR(struct node *node, pid_t pid)
{
	struct node *found;
	
	if (node == NULL)
		return NULL;

	if (node->p->pid == pid)
		return node;

	found = getNodeR(node->child, pid);
	if (found != NULL)
		return found;

	found = getNodeR(node->next, pid);
	if (found != NULL)
		return found;

	return NULL;
}

static void
prettyPrintNode(struct node *node, int level)
{
	while(level-- > 0)
		printf("\t");
	printf("--- ppid: %ld pid: %ld cmd: %s ---\n",
	       (long) node->p->ppid, (long) node->p->pid, node->p->cmd);
}
