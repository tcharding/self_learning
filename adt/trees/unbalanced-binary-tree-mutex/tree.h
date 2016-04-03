#ifndef TREE_H
#define TREE_H

#include <pthread.h>

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

	pthread_mutex_t *mutex;

	struct node *left;
	struct node *right;
};

struct node *add(struct node *root, char *key, void *value);
Boolean delete(struct node *root, char *key);
Boolean lookup(struct node *root, char *key, void **value);
void freeNode(struct node *root, void (*fn)(void *));

#endif	/* TREE_H */
