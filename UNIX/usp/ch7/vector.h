#ifndef TCH_VECTOR_H
#define TCH_VECTOR_H

/* 
 * Dynamic Vector Library
 */

typedef struct {
	int slots;
	int cnt;
	data_t **data;
} VECTOR;

typedef void Freefunc(data_t *); /* free data_t */
typedef int Func(data_t *);	 /* generic function for data_t */

VECTOR *v_creat();
void v_free(VECTOR *v, Freefunc *fn);
int v_add(VECTOR *v, data_t *d);
int v_foreach(VECTOR *vp, Func *fn);

#endif	/* TCH_VECTOR_H */
