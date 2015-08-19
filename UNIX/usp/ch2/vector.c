#include "vector.h"
#include <string.h>
#include <stdlib.h>
#define INIT_SIZE 1

/* creatdv: create and initialise dv */
struct dv *creatdv(void)
{
	struct dv *p;

	if ((p = malloc(sizeof(struct dv))) != NULL) {
		p->size = 0;
		p->cnt = 0;
		p->v = NULL;
	}
	return p;
}

/* freedv: free resources allocated with creatdv() */
void freedv(struct dv *p)
{

	if (p)
		freev(p->v);
	free(p);
}

/* adds: add s to p, return new count or -1 on error */
int adds(struct dv *p, const char *s)
{
	char *new_s, **new_v;

	if (p == NULL || s == NULL) 
		return -1;

	if (p->v == NULL) {	/* first time through */
		p->v = malloc(INIT_SIZE * sizeof(char *) + 1); /* +1 for NULL */
		if (p->v == NULL)
			return -1;
		p->size = INIT_SIZE;
	} else if (p->cnt >= p->size) { /* re size */
		new_v = realloc(p->v, p->size * 2 + 1); /* +1 for NULL */
		if (new_v == NULL)
			return -1;
		p->v = new_v;
		p->size *= 2;
	}
	if ((new_s = malloc(strlen(s) + 1)) == NULL)
		return -1;
	p->v[p->cnt++] = strcpy(new_s, s);
	p->v[p->cnt] = NULL;
	return p->cnt;
}

/* foreach: call fnc on each member of dv, 
return accumulated total from function calls or -1 on error */
int foreach(struct dv *p, int (*fnc)(char *))
{
	char **v;
	int tot;

	tot = 0;
	if (p == NULL || p->v == NULL)
		return -1;
	
	for (v = p->v; *v != NULL; ++v)
		tot += (*fnc)(*v);
	return tot;
}

/* dupv: return duplicate of p->v, free with freev() */
char **dupv(struct dv *p)
{
	char **new_v, **new_vp, **vp, *new_s;

	if (p == NULL || p->v == NULL)
		return NULL;

	new_v = malloc(p->cnt * sizeof(char *) + 1); /* +1 for NULL */
	if (new_v == NULL)
		return NULL;

	new_vp = new_v;		/* save front */
	for (vp = p->v; *vp != NULL; ++vp) {
		new_s = malloc(strlen(*vp) + 1);
		if (new_s == NULL) {
			*new_vp = NULL; /* add terminator so we can freev */
			freev(new_v);
			return NULL;
		} else
			*new_vp++ = strcpy(new_s, *vp);
	}
	*new_vp = NULL;		/* NULL terminator */
	return new_v;
}

void freev(char **v)
{
	char **p;
	
	if (v) 
		for (p = v ; *p != NULL; p++)
			free(*p);
	free(v);		  /* free the array */
}
