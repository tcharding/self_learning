#include "dia.h"
#include <stdio.h>
#include <stdlib.h>

#define ALLOC 1			/* set to 1 to test */

struct dia *dia_creat(void)
{
	struct dia *p;

	if ((p = malloc(sizeof(struct dia))) == NULL) {
		fprintf(stderr, "dia_creat: malloc error\n");
		return NULL;
	}
	p->size = 0;
	p->num = 0;
	p->data = NULL;
	return p;
}

void dia_free(struct dia *p)
{
	free(p->data);
	free(p);
}
int dia_add(struct dia *p, int val)
{
	int memsize;
	
	if (p == NULL)
		return -1;
	if (p->size == 0) {	/* first time through */
		memsize = ALLOC;
		if ((p->data = malloc(sizeof(int) * memsize)) == NULL) {
			fprintf(stderr, "dia_add: malloc error\n");
			return NULL;
		}
		p->size = memsize;
	} else if (p->num >= p->size) { /* resize */
		int *temp;
		memsize = p->size * 2;
		if ((temp = realloc(p->data, sizeof(int) * memsize)) == NULL) {
			fprintf(stderr, "dia_add: realloc error\n");
			return NULL;
		} else {
			p->data = temp;
			p->size = memsize;
		}
	}
	p->data[p->num++] = val;
	return p->num;
}
void dia_print(struct dia *p)
{
	int i;

	for (i = 0; i < p->num; i++)
		printf( (i < p->num-1) ? "%d, " : "%d", p->data[i]);
}
