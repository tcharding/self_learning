/* unbalanced binary tree

  key is unique across tree, null value indicates item (key) has been deleted.

*/
#include <stdlib.h>
#include <unistd.h>
#include <stdio.h>
#include <string.h>
#include "tree.h"

/* add: add key/value to root */
struct node *
add(struct node *root, char *key, void *value)
{
	
	if (root == NULL) {
		root = malloc(sizeof(struct node));
		if (root == NULL)
			return NULL;
		root->key = key;
		root->value = value;
		root->left = NULL;
		root->right = NULL;

		return root;
	}

	if (strcmp(root->key, key) == 0) {
		if (root->value != NULL)
			return NULL; /* Error: key already present */
		root->value = value;
	}

	if (strcmp(root->key, key) < 0) {
		add(root->left, key, value);
	} else {
		add(root->right, key, value);
	}

	return root;
}

/* delete: delete value associated with key from root 
    return TRUE if node deleted, FALSE otherwise */
Boolean
delete(struct node *root, char *key)
{
	if (root == NULL)
		return FALSE;

	if (strcmp(root->key, key) == 0) {
		if (root->value == NULL)
			return FALSE;
		root->value = NULL;
		return TRUE;
	}

	if (strcmp(root->key, key) < 0)
		return delete(root->left, key);
	else
		return delete(root->right, key);

	return FALSE;		/* shouldn't get here */
}

/* lookup: get value for key, returns true if key exists 
    value is only modified if key exists */
Boolean
lookup(struct node *root, char *key, void **value)
{
	if (root == NULL)
		return FALSE;

	if (strcmp(root->key, key) == 0) {
		if (root->value != NULL)
			return TRUE;
		else
			return FALSE; /* node deleted */
	}

	if (strcmp(root->key, key) < 0)
		return lookup(root->left, key, value);
	else
		return lookup(root->right, key, value);

	return FALSE;		/* shouldn't get here */
}

/* test implementation */
static void fail(const char *msg);
static void testAdd(void);
static void testDelete(void);
static void testLookup(void);

int
main(void) {
	testAdd();
	testDelete();
	testLookup();

	exit(EXIT_SUCCESS);
}

static void testAdd(void)
{
	struct node *root;

	root = NULL;

	root = add(root, strdup("key"), strdup("value"));
	if (root == NULL)
		fail("initial add");

	if (add(root, strdup("key"), strdup("value")) != NULL)
		fail("added same key twice");

	root = add(root, strdup("another"), strdup("value"));
	if (root == NULL)
		fail("add second node");

}
static void testDelete(void)
{
	struct node *root;

	root = NULL;

	root = add(root, strdup("key"), strdup("value"));
	if (root == NULL)
		fail("initial add");

	if (add(root, strdup("key"), strdup("value")) != NULL)
		fail("added same key twice");

	root = add(root, strdup("another"), strdup("value"));
	if (root == NULL)
		fail("add second node");

	if (delete(root, "key") != TRUE)
		fail("couldn't remove present node");

	if (delete(root, "key") == TRUE)
		fail("removed non-present node");
	
}
static void testLookup(void)
{
	struct node *root;
	char *value;

	root = NULL;

	root = add(root, strdup("key"), strdup("value"));
	if (root == NULL)
		fail("initial add");

	if (lookup(root, "key", NULL) != TRUE)
		fail("couldn't find present node with NULL value");

	if (lookup(root, "key", &value) != TRUE)
		fail("couldn't find present node");
	if (strcmp(value, "value") != 0)
		fail("didn't get value");

	if (lookup(root, "not there", NULL) == TRUE)
		fail("found non-existant node");

	root = add(root, strdup("another"), strdup("value"));
	if (root == NULL)
		fail("add second node");

	if (delete(root, "key") != TRUE)
		fail("couldn't remove present node");

	value = "don't touch me"
	if (lookup(root, "key", &value) == TRUE)
		fail("found deleted node");
	if (strcmp(value, "don't touch me") != 0)
		fail("lookup modified value when it should not have");
}

static void fail(const char *msg)
{
	fprintf(stderr, "Fail: %s\n", msg);
	exit(1);
}

