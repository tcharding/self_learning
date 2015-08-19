#ifndef _STACK_H
#define _STACK_H

struct stack {
	int size;		/* size of memory allocated */
	int sp;			/* next position to fill */
	char **v;		/* data */
};

struct stack *st_creat();
void st_free(struct stack *);
int st_push(struct stack *, const char *);
char *st_pop(struct stack *);
char *dups(const char *s);

#endif	/* _STACK_H */
