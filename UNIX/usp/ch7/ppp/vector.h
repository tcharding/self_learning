#ifndef TCH_VECTOR_H
#define TCH_VECTOR_H
#include "data.h"
/* 
 * Dynamic Vector Library
 */

typedef struct vector {
	int slots;
	int cnt;
	data_t **data;		/* NULL terminated */
} vec_t;

typedef void Freefunc(data_t *); /* free data_t */
typedef int Func(data_t *);	 /* generic function for data_t */

vec_t *v_creat();
void v_init(vec_t *v);
void v_free(vec_t *v, Freefunc *fn);
int v_add(vec_t *v, data_t *d);
int v_foreach(vec_t *vp, Func *fn);

#endif	/* TCH_VECTOR_H */
