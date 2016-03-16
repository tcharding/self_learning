#include "node.h"
#include "tlpi_hdr.h"

static struct node *getNode(struct node *root, int id);
static void printTreeR(struct node *node, int level);

static struct node *getNodeR(struct node *node, int id);
static void prettyPrintNode(struct node *node, int level);

/* node: allocate memory and create node */
struct node *node(int id)
{
	struct node *node;

	node = malloc(sizeof(struct node));
	if (node == NULL) {
		errno = ENOMEM;
		return NULL;
	}
	node->id = id;
	node->child = NULL;
	node->next = NULL;

	return node;
}

/* freeNode: free node created with node() */
void freeNode(struct node *ptr)
{
				/* TODO */
}

/* addTreeParentIdChild: add child node to parentId starting at root
    return non-zero if parent not found */
int
addTreeParentIdChild(struct node *root, int parentId, struct node *child)
{
	struct node *parent, *ptr;

	parent = getNode(root, parentId);
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

/* getNode: return node with id, NULL if node not found */
static struct node *
getNode(struct node *root, int id)
{
	return getNodeR(root, id);
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
getNodeR(struct node *node, int id)
{
	struct node *found;
	
	if (node == NULL)
		return NULL;

	if (node->id == id)
		return node;

	found = getNodeR(node->child, id);
	if (found != NULL)
		return found;

	found = getNodeR(node->next, id);
	if (found != NULL)
		return found;

	return NULL;
}

static void
prettyPrintNode(struct node *node, int level)
{
	while(level-- > 0)
		printf("\t");
	printf("--- %d ---\n", node->id);
}
