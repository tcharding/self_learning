#ifndef VECTOR_H
#define VECTOR_H

typedef struct vector {
	int slots;		
	int count;		/* number of elements */
	char **dp;		/* data */
} vec_t;

vec_t *sv_creat();
void sv_free(vec_t *vt)
int sv_add(const char *s);
char *dups(const char *s);

#endif	/* VECTOR_H */
