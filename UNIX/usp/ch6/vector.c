#include "usp.h"
#include "vector.h"
#include <stdlib.h>
#define INIT 1

/* 
 * Dynamic vector of character pointers (strings)
 */

/* sv_creat: allocate memory for vector and initialise */
vec_t *sv_creat()
{
	vec_t *vt;
	
	if ((vt = malloc(sizeof(vec_t))) == NULL)
		return NULL;
	vt->slots = 0;
	vt->sp = 0;
	vt->dp = NULL;
	return vt;
}

/* sv_add: add s to end of vector, return index */
int sv_add(vec_t *vt, const char *s)
{
	int slots;
	
	if (vt == NULL || s == NULL) {
		errno = EINVAL;
		return -1;
	}

	if (vt->dp == NULL) {	/* first time through */
		slots = INIT;
		if ((vt->dp = calloc(slots, sizeof(char *))) == NULL)
			err_sys("calloc failed");
		vt->slots = slots;
	} else if (vt->count >= vt->slots ) { /* re-size */
		char **tmp;
		slots = vt->slots * 2;
		if ((tmp = realloc(vt->dp, slots*sizeof(char *))) == NULL)
			err_sys("realloc failed");
		vt->slots = slots;
		vt->dp = tmp;
	}

	if (ms_open(&m) == -1) 
		err_sys("m_open_mstream failed");
	if (fputs(s, m.stream) == EOF)
		err_sys("fputs failed: %s", s);
	fclose(m.stream);
	vt->dp[vt->sp++] = m.buf;
	return vt->sp;
}

/* dups: duplicate s using memory streams, NULL on error with errno set */
char *dups(const char *s)
{
	FILE *fp;		
	int size;
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
