#include "node.h"
#include "tlpi_hdr.h"

static struct node *buildTree(void);

int
main(void)
{
	struct node *root;

	root = buildTree();
	printTree(root);
	free(root);
	
	exit(EXIT_SUCCESS);
}

static struct node *buildTree(void)
{
	struct node *root, *ptr;

	root = node(1);

	ptr = node(11);
	if (addTreeParentIdChild(root, 1, ptr))
		errExit("add");
	ptr = node(12);
	if (addTreeParentIdChild(root, 1, ptr))
		errExit("add");
	ptr = node(13);
	if (addTreeParentIdChild(root, 1, ptr))
		errExit("add");

	ptr = node(111);
	if (addTreeParentIdChild(root, 11, ptr))
		errExit("add");
	ptr = node(112);
	if (addTreeParentIdChild(root, 11, ptr))
		errExit("add");
	ptr = node(113);
	if (addTreeParentIdChild(root, 11, ptr))
		errExit("add");

	ptr = node(131);
	if (addTreeParentIdChild(root, 13, ptr))
		errExit("add");
	ptr = node(132);
	if (addTreeParentIdChild(root, 13, ptr))
		errExit("add");

	return root;
}
