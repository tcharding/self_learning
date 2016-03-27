#ifndef STACK_H
#define STACK_H

#include <sys/types.h>

struct stack;

struct stack *newStack(void);
void freeStack(struct stack *stack);

static int push(struct stack *sp, const char *s);
static char *pop(struct stack *sp, char *buf, size_t bufsize);
static char *show(struct stack *sp, char *buf, size_t bufsize);

#endif	/* STACK_H */
