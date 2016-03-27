/* stack of strings, linked list implementation */
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include "stack.h"

#ifndef BUF_SIZE
#define BUF_SIZE 1064
#endif

struct stack {
	struct node *sp;
};

struct node {
	char *s;
	struct node *next;
};

static void freeNode(struct node *ptr);

/* newStack: initialise stack */
struct stack *newStack(void)
{
	struct stack *stack;

	stack = malloc(sizeof(struct stack));
	if (stack == NULL)
		return NULL;

	stack->sp = NULL;
	return stack;
}

/* freeStack: free memory allocated with newStack */
void
freeStack(struct stack *stack)
{
	if (stack != NULL) {
		if (stack->sp != NULL)
			freeNode(stack->sp);
		free(stack);
	}
}

/* push: push copy of s onto stack */
static int
push(struct stack *stack, const char *s)
{
	struct node *node;

	if (stack == NULL || s == NULL)
		return -1;

	node = malloc(sizeof(struct node));
	if (node == NULL)
		return -1;

	node->s = strdup(s);
	if (node->s == NULL)
		return -1;	/* errno set by strdup */
	node->next = NULL;
	
	if (stack->sp == NULL) {
		stack->sp = node;
	} else {
		node->next = stack->sp;
		stack->sp = node;
	}
	return 0;
}

/* pop: pop top of stack. Copy into buf if buf is non-null. */
static char *
pop(struct stack *stack, char *buf, size_t bufsize)
{
	struct node *node;
	
	if (stack == NULL)
		return NULL;

	if (stack->sp == NULL)
		return NULL;

	node = stack->sp;
	stack->sp = node->next;

	if (buf != NULL) {
		strncpy(buf, node->s, bufsize-1);
		if (bufsize > 0)
			buf[bufsize - 1] = '\0';
	}

	freeNode(node);
	
	return buf;
}

/* show: get top of stack without removing */
static char *
show(struct stack *stack, char *buf, size_t bufsize)
{
	if (stack == NULL || stack->sp == NULL || buf == NULL)
		return NULL;

	strncpy(buf, stack->sp->s, bufsize-1);
	if (bufsize > 0)
		buf[bufsize - 1] = '\0';

	return buf;
}
static void
freeNode(struct node *node)
{
	struct node *next;
	
	if (node != NULL) {
		next = node->next;
		if (node->s != NULL)
			free(node->s);
		free(node);
		freeNode(next);
	}
}

