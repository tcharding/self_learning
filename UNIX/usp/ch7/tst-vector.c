#include "tch.h"
#include "vector.h"
#include "data.h"
#include "string.h"
#define NUM 3			/* test all paths through v_add */

int test_fn(data_t *d);

/* test vector implementation */
int main(void)
{
	VECTOR *vp;
	data_t d, *dp;
	int i, cnt;
				/* test creat, free */
	if ((vp = v_creat()) == NULL)
		err_quit("v_creat error");
	v_free(vp, adt_free);

					/* add data_t from stack */
	if ((vp = v_creat()) == NULL)
		err_quit("v_creat error");

	for (i = 0; i < NUM; i++) {
		d.c = (char)i;
		d.s = s_dup("dummy string");
		dp = adt_dup(&d);
		if ((cnt = v_add(vp, dp)) != i+1)
			err_msg("v_add error: cnt:%d v->cnt:%d", cnt, vp->cnt);
		free(d.s);
	}
	v_free(vp, adt_free);

					/* add data_t from pointer and print */
	if ((vp = v_creat()) == NULL)
		err_quit("v_creat error");

	for (i = 0; i < NUM; i++) {
		if ((dp = adt_creat('a', "tstou oau")) == NULL)
			err_sys("adt_creat error");
		if ((cnt = v_add(vp, dp)) != i+1)
			err_msg("v_add error: cnt:%d v->cnt:%d", cnt, vp->cnt);
	}
	if (v_foreach(vp, test_fn) != 0)
		err_sys("foreach not zero");
	v_free(vp, adt_free);

	return 0;
}

int test_fn(data_t *d)
{
	char *s;

	if ((s = adt_tostring(d)) == NULL)
		return -1;
	fprintf(stderr, "test_fn: %s\n", s);
	free(s);
	return 0;
}


