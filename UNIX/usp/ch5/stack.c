#include "tch.h"
#include "stack.h"
#include <errno.h>
#define INIT 1

/* 
 * Stack of strings
 */

/* st_creat: allocate memory for stack and initialise */
struct stack *st_creat()
{
	struct stack *st;
	
	if ((st = malloc(sizeof(struct stack))) == NULL)
		return NULL;
	st->size = 0;
	st->sp = 0;
	st->v = NULL;
	return st;
}

/* st_free: free memory created with st_creat */
void st_free(struct stack *st)
{
	int i;

	if (st == NULL)
		return;
	if (st->v) {
		for (i = 0; i < st->sp; i++) {
			free(st->v[i]);
		}
	}
	free(st);
}

/* st_push: push s to top of stack, return new stack size */
int st_push(struct stack *st, const char *s)
{
	int size;
	
	if (st == NULL || s == NULL)
		return -1;

	if (st->v == NULL) {	/* first time through */
		size = INIT * sizeof(char *);
		if ((st->v = malloc(size)) == NULL)
			err_sys("malloc failed");
		st->size = size;
	} else if (st->sp >= st->size ) { /* re-size */
		char **tmp;
		size = st->size * 2 * sizeof(char *);
		if ((tmp = realloc(st->v, size)) == NULL)
			err_sys("realloc failed");
		st->size = size;
		st->v = tmp;
	}
	st->v[st->sp++] = dups(s);
	return st->sp;
}

char *st_pop(struct stack *st)
{
	char *s = NULL;
	
	if (st->sp != 0) 
		s = st->v[--st->sp];

	return s;
}

/* dups: duplicate s using memory streams, NULL on error with errno set */
char *dups(const char *s)
{
	FILE *fp;		
	size_t size;
	char *buf;
	int e;
	
	if ((fp = open_memstream(&buf, &size)) == NULL)
		return NULL;
	if (fputs(s, fp) < 0) {
		e = errno, free(buf), errno = e;
		return NULL;
	}
	if (fclose(fp) != 0) {
		e = errno, free(buf), errno = e;
		return NULL;
	}
	return buf;
}
