#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include "dvString.h"

#define INIT_SIZE 1

/* dvInit: initialise dynamic vector */
struct dv *
dvInit(void)
{
	struct dv *dvp;

	dvp = malloc(sizeof(struct dv));
	if (dvp == NULL)
		return NULL;

	dvp->v = calloc(INIT_SIZE+1, sizeof(char *));
	if (dvp->v == NULL)
		return NULL;
	*(dvp->v) = NULL;
	dvp->size = INIT_SIZE;
	dvp->count = 0;
	
	return dvp;
}

/* dvDestroy: destroy dvp, calls free on all strings */
void
dvDestroy(struct dv *dvp)
{
	char **v;
	
	if (dvp != NULL) {
		if (dvp->v != NULL) {
			for (v = dvp->v; *v != NULL; v++)
				free(*v);
			free(dvp->v);
		}
		free(dvp);
	}
}

/* dvAdd: add s to dvp, return current count or -1 on error */
int
dvAdd(struct dv *dvp, char *s)
{
	char **v;
	int numSlots;

	if (dvp->count >= dvp->size) {	      /* increase dvp->v */
		numSlots = dvp->size * 2 + 1; /* +1 for NULL */
		v = realloc(dvp->v, numSlots * sizeof(char *));
		if (v == NULL)
			return -1; /* realloc sets errno */
		dvp->v = v;
		dvp->size = numSlots - 1;
	}

	for (v = dvp->v; *v != NULL; v++)
		;		/* seek to end */

	*v = s;
	v++;
	*v = NULL;
	dvp->count++;

	return dvp->count;
}


/* test implementation */
int
main(void)
{
	struct dv *dvp;
	char *s;
	char **v;

	dvp = dvInit();

	s = strdup("first string");
	dvAdd(dvp, s);
	s = strdup("second string");
	dvAdd(dvp, s);
	s = strdup("third string");
	dvAdd(dvp, s);

	for (v = dvp->v; *v != NULL; v++) {
		printf("%s\n", *v);
	}
	dvDestroy(dvp);

	exit(EXIT_SUCCESS);
}
