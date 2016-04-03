#ifndef TREE_H
#define TREE_H

#ifdef TRUE
#undef TRUE
#endif

#ifdef FALSE
#undef FALSE
#endif

typedef enum { FALSE, TRUE } Boolean;

struct node {
	char *key;
	void *value;

	struct node *left;
	struct node *right;
};

struct node *add(struct node *root, char *key, void *value);
Boolean delete(struct node *root, char *key);
Boolean lookup(struct node *root, char *key, void **value);

#endif	/* TREE_H */
