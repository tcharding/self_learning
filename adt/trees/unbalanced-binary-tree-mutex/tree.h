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

	pthread_mutex_t *mutex;
	void *value;

	struct node *left;
	struct node *right;
};

int initialize(struct node *root);
int add(struct node *root, char *key, void *value);
int delete(struct node *root, char *key);
Boolean lookup(char *key, void **value);

#endif	/* TREE_H */
