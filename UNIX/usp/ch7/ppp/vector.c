#include "tch.h"
#include "vector.h"
#include <string.h>
#include <stdlib.h>
#define INIT_SLOTS 1

/* v_creat: allocate memory and initialise vector
   free with v_free */
vec_t *v_creat()
{
	vec_t *v;
	
	if ((v = malloc(sizeof(vec_t))) == NULL)
		return NULL;
	v->slots = 0;
	v->cnt = 0;
	/* bzero(*v->data, sizeof(data_t ***)); */
	v->data = NULL;
	return v;
}

/* v_free: free memory allocated with v_creat */
void v_free(vec_t *v, Freefunc *func)
{
	int i;
	data_t *dp;
	
	if (v == NULL || func == NULL)
		return;
	if (v->data != NULL) {
		for (i = 0; i < v->cnt; i++) {
			dp = v->data[i];
			func(dp);
		}
		free(v->data);
	}
	free(v);
}

/* v_init: initialise vector */
void v_init(vec_t *v)
{
	v->slots = 0;
	v->cnt = 0;
	v->data = NULL;
}
/* v_add: add data to v, return new count or -1 on error */
int v_add(vec_t *v, data_t *d)
{
	data_t **new;
	int nbytes;

	if (v == NULL || d == NULL) {
		errno = EINVAL;
		return -1;
	}

	if (v->data == NULL) {	/* first time through */
		nbytes = (INIT_SLOTS +1) * sizeof(data_t *); /* +1 for NULL */
		if ((v->data = malloc(nbytes)) == NULL)
			return -1;
		bzero(v->data, nbytes);
		v->slots = INIT_SLOTS;
	} else if (v->cnt >= v->slots) { /* re-size */
		nbytes = (v->slots * 2 + 1) * sizeof(data_t *); /* +1 for NULL */
		if ((new = realloc(v->data, nbytes)) == NULL)
			return -1;
		v->data = new;
		v->slots *= 2;
	}
	v->data[v->cnt] = d;
	++v->cnt;
	v->data[v->cnt] = NULL;	/* NULL terminator */
	return v->cnt;
}

/* foreach: call fnc on each member of dv,
return accumulated total from function calls or -1 on error */
int v_foreach(vec_t *v, Func *func)
{
	data_t *d;
	int total, res;
	int i;

	total = 0;
	if (v == NULL || v->data == NULL || func == NULL) {
		errno = EINVAL;
		return -1;
	}
	for (i = 0; i < v->cnt; i++) {
		d = v->data[i];
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
