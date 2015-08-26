#include "tch.h"
#include "vector.h"
#include "string.h"
#define NUM 3			/* test all paths through v_add */

static void dump_data_t(data_t *d);
static void free_d(data_t *d);

/* test vector implementation */
int main(void)
{
	VECTOR *vp;
	data_t d;
	int i, cnt;
	
	if ((vp = v_creat()) == NULL)
		err_quit("v_creat error");

	for (i = 0; i < NUM; i++) {
		d.n = i;
		d.s = dups("dummy string");
		if ((cnt = v_add(vp, &d)) != i+1)
			err_msg("v_add error: cnt:%d v->cnt:%d", cnt, vp->cnt);
		free(d.s);
	}
	fprintf(stderr, "Dumping VECTOR\n");
	for (i = 0; i < NUM; i++) {
		dump_data_t(vp->data[i]);
		fprintf(stderr, "\n");
	}
	free(vp);
	/* v_free(vp, free_d); */
	return 0;
}

static void dump_data_t(data_t *d)
{
	char *s;

	if (d == NULL)
		return;
	
	s = (d->s == NULL) ? "NULL" : d->s;
	fprintf(stderr, "data_t: [%d %s]",
		d->n, s);
}

static void free_d(data_t *d)
{
	if (d == NULL)
		return;
	if (d->s != NULL)
		free(d->s);
	free(d);
}
