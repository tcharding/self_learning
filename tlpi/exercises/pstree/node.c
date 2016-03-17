#include "pstree.h"
#include "tlpi_hdr.h"

struct proc {
	pid_t ppid;
	pid_t pid;
	char *cmd;
};

static struct node *findNode(struct node *root, pid_t pid);
static void freeNodeR(struct node *ptr);
static void printTreeR(struct node *node, int level);

static size_t treeSizeR(struct node *node);
static struct node *findNodeR(struct node *node, pid_t pid);
static struct proc *newProc(pid_t ppid, pid_t pid, const char *cmd);
static void freeProc(struct proc *p);
static void prettyPrintNode(struct node *node, int level);

/* tests */
static void testFindNode(void);
static void testCanAdd(void);
static void testFreeTree(void);
static void testAddChild(void);

/* node: allocate memory and create node 
    returns NULL and sets errno on error */
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

/* addChild: add child to root 
    return non-zero if parent not found */
int
addChild(struct node **root, struct node *child)
{
	struct node *parent, *ptr;

	if (root == NULL || child == NULL)
		return 1;
	
	if (*root == NULL) {
		*root = child;
		return 0;
	}

	parent = findNode(*root, child->p->ppid);
	if (parent == NULL)
		return 2;

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

/* addProc: create child and add to root
    return non-zero on error */
int
addProc(struct node **root, pid_t ppid, pid_t pid, const char *cmd)
{
	struct node *n;

	n = node(ppid, pid, cmd);
	if (n == NULL)
		return 1;

	return addChild(root, n);
}

/* freeTree: free tree starting at root */
void
freeTree(struct node *root)
{
	freeNodeR(root);
}

/* printTree: pretty print tree */
void printTree(struct node *root)
{
	printTreeR(root, 0);
}

/* printTree: pretty print siblings of this node */
void printSiblings(struct node *n)
{
	while (n != NULL) {
		prettyPrintNode(n, 0);
		n = n->next;
	}
}

/* canAdd: true if child->ppid is already in tree */
Boolean
canAdd(struct node *root, struct node *child)
{
	struct node *found;

	if (root == NULL || child == NULL)
		return FALSE;
	
	found = findNode(root, child->p->ppid);
	if (found == NULL)
		return FALSE;

	return TRUE;
}

/* return number of nodes in the tree */
size_t
treeSize(struct node *root)
{
	return treeSizeR(root);
}

/* findNode: return node with id, NULL if node not found */
static struct node *
findNode(struct node *root, pid_t pid)
{
	return findNodeR(root, pid);
}

/* freeNodeR: free node created with node() */
static void
freeNodeR(struct node *node)
{
	if (node == NULL)
		return;

	freeNodeR(node->child);
	freeNodeR(node->next);
	
	freeProc(node->p);
	free(node);
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

/* treeSizeR: recursive helper */
size_t
treeSizeR(struct node *node)
{
	size_t total = 0;
	
	if (node == NULL)
		return total;

	total += treeSizeR(node->child);
	total += treeSizeR(node->next);

	return ++total;
}

/* findNodeR: recursive helper */
static struct node *
findNodeR(struct node *node, pid_t pid)
{
	struct node *found = NULL;
	
/*	fprintf(stderr, "findNodeR looking for: %ld\n", (long) pid); */

	if (node == NULL)
		return NULL;

	
	if (node->p->pid == pid)
		return node;

	found = findNodeR(node->child, pid);
	if (found != NULL)
		return found;

	found = findNodeR(node->next, pid);
	if (found != NULL)
		return found;

	return NULL;
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

/* freeProc: helper function called by freeNodeR. Don't call this directly */
static void
freeProc(struct proc *p)
{
	if (p != NULL) {
		if (p->cmd != NULL)
			free(p->cmd);
		free(p);
	}
}

static void
prettyPrintNode(struct node *node, int level)
{
	if (node == NULL)
		return;
	
	while(level-- > 0)
		fprintf(stderr, "\t");
	fprintf(stderr, "(%ld): %s", (long) node->p->pid, node->p->cmd);
	       
}


/* testTree: build a test tree */
struct node *
buildTestTree(void)
{
	struct node *root = NULL;

	if (addProc(&root, 0, 1, "init"))
		errExit("add");

	if (addProc(&root, (pid_t) 1, (pid_t) 11, "win"))
		errExit("add");
	if (addProc(&root, (pid_t) 1, (pid_t) 12, "loose"))
		errExit("add");
	if (addProc(&root, (pid_t) 1, (pid_t) 13, "draw"))
		errExit("add");

	if (addProc(&root, (pid_t) 11, (pid_t) 111, "profit"))
		errExit("add");
	if (addProc(&root, (pid_t) 11, (pid_t) 112, "zsh"))
		errExit("add");
	if (addProc(&root, (pid_t) 11, (pid_t) 113, "zsh"))
		errExit("add");

	if (addProc(&root, (pid_t) 13, (pid_t) 131, "zsh"))
		errExit("add");
	if (addProc(&root, (pid_t) 13, (pid_t) 132, "zsh"))
		errExit("add");

	return root;
}

/************** TESTING **************/

void
testNode(void)
{
	testFreeTree();
	testFindNode();
	testCanAdd();
	testAddChild();
}

static void
testFindNode()
{
	struct node *root = NULL;

	if (findNode(root, (pid_t)1)) {
		fprintf(stderr, "false negative\n");
	} 

	root = buildTestTree();

	/* test find existing node */
	if (!findNode(root, (pid_t)111)) {
		fprintf(stderr, "find failed\n");
	} 
	if (!findNode(root, (pid_t)132)) {
		fprintf(stderr, "find failed\n");
	} 

	/* test find non-existent node */
	if (findNode(root, (pid_t)1236)) {
		fprintf(stderr, "false negative\n");
	} 
}

static void
testCanAdd(void)
{
	struct node *root, *n;

	root = buildTestTree();

	n = node((pid_t) 1234, (pid_t) 1111, "zsh");
	if (n == NULL)
		errExit("add");

	if (canAdd(root, n))
		fprintf(stderr, "false positive\n");

	n = node((pid_t) 11, (pid_t) 1111, "zsh");
	if (n == NULL)
		errExit("add");

	if (!canAdd(root, n))
		fprintf(stderr, "false negative\n");
}

static void
testFreeTree(void)
{
	struct node *root = NULL;

	freeTree(root);

	root = node((pid_t) 0, (pid_t) 1, "zsh");
	freeTree(root);

	root = buildTestTree();
	freeTree(root);
}

static void
testAddChild(void)
{
	struct node *root, *n;

	root = node((pid_t) 0, (pid_t) 1, "root");

	n = node((pid_t) 1, (pid_t) 12, "zsh");
	if (addChild(&root, n))
		errExit("addChild");

	if (treeSize(root) != 2)
		errExit("treeSize 1");
/*

	ptr = ptr->next;
	ptr->next = node((pid_t) 1, (pid_t) 12, "loose");
	ptr = ptr->next;
	ptr->next = node((pid_t) 1, (pid_t) 13, "draw");
	ptr = ptr->next;
*/
	
}
