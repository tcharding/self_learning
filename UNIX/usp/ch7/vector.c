#include "vector.h"
#include <string.h>
#include <stdlib.h>
#define INIT_SLOTS 1

/* v_creat: allocate memory and initialise vector
   free with v_free */
VECTOR *v_creat()
{
	VECTOR *v;
	
	if ((v = malloc(sizeof(VECTOR))) == NULL)
		return NULL;
	v->slots = 0;
	v->cnt = 0;
	/* bzero(*v->data, sizeof(data_t ***)); */
	v->data = NULL;
	return v;
}

/* v_free: free memory allocated with v_creat */
void v_free(VECTOR *v, Freefunc *func)
{
	int i;
	
	if (v == NULL || func == NULL)
		return;
	if (v->data != NULL) {
		for (i = 0; i < v->cnt; i++)
			func(v->data[i]);
	}
	free(v);
}

/* v_add: add data to v, return new count or -1 on error */
int v_add(VECTOR *v, data_t *d)
{
	data_t **new;

	if (v == NULL || d == NULL) 
		return -1;

	if (v->data == NULL) {	/* first time through */
		v->data = malloc(INIT_SLOTS * sizeof(data_t *) + 1); /* +1 for NULL */
		if (v->data == NULL)
			return -1;
		v->slots = INIT_SLOTS;
	} else if (v->cnt >= v->slots) { /* re size */
		new = realloc(v->data, v->slots * 2 + 1); /* +1 for NULL */
		if (new == NULL)
			return -1;
		v->data = new;
		v->slots *= 2;
	}
	v->data[v->cnt] = d;
	return ++v->cnt;
}

/* foreach: call fnc on each member of dv,
return accumulated total from function calls or -1 on error */
int foreach(VECTOR *v, Func *func);
{
	data_t **d;
	int total, res;

	total = 0;
	if (v == NULL || v->data == NULL || func == NULL)
		return -1;

	for (i = 0; i < v->cnt; i++) {
		d = vp->data[i];
		if (d == NULL)
			continue; /* allow for future implementation of holes */
		res = func(d);
		if (res == -1)
			return -1;
		else
			total += res;
	}
	return total;
}

/* /\* dupv: return duplicate of p->v, free with freev() *\/ */
/* char **dupv(data_t *p) */
/* { */
/* 	char **new_v, **new_vp, **vp, *new_s; */

/* 	if (p == NULL || p->v == NULL) */
/* 		return NULL; */

/* 	new_v = malloc(p->cnt * sizeof(char *) + 1); /\* +1 for NULL *\/ */
/* 	if (new_v == NULL) */
/* 		return NULL; */

/* 	new_vp = new_v;		/\* save front *\/ */
/* 	for (vp = p->v; *vp != NULL; ++vp) { */
/* 		new_s = malloc(strlen(*vp) + 1); */
/* 		if (new_s == NULL) { */
/* 			*new_vp = NULL; /\* add terminator so we can freev *\/ */
/* 			freev(new_v); */
/* 			return NULL; */
/* 		} else */
/* 			*new_vp++ = strcpy(new_s, *vp); */
/* 	} */
/* 	*new_vp = NULL;		/\* NULL terminator *\/ */
/* 	return new_v; */
/* } */

