#include "tch.h"
#include "data.h"
#include "string.h"

/* implicitly tests string.c */

/* integration tests for data.c (data_t implementation) */
int main(void)
{
	data_t d; 
	data_t *dp;
	char *s;
				/* test alloc and free */
	err_msg("Starting tests ...\n");
	dp = NULL;
	if((dp = adt_alloc()) == NULL)
		err_sys("adt_alloc error");
	adt_free(dp);

				/* test string and adt_err_* */
	d.c = 'c';
	d.s = s_dup("s_dup string");
				/* test error functions */
	/* adt_err_msg(&d, "forced error"); */
	/* fprintf(stderr, "but we continue anyway ... exiting \n"); */
	/* errno = EINVAL; */
	/* adt_err_sys(&d, "forced error"); */
	free(d.s);

				/* test adt_tostring */
	d.c = 'c';
	d.s = s_dup("s_dup string");
	if ((s = adt_tostring(&d)) == NULL)
		adt_err_sys(&d, "adt_creat error");
	fprintf(stderr, "We got: %s\n", s);
	free(d.s);
	free(s);
				/* test creat, dup, */
	if ((dp = adt_creat('c', "string")) == NULL)
		err_sys("adt_creat error");

	if ((s = adt_tostring(dp)) == NULL)
		adt_err_sys(dp, "adt_creat error");
	fprintf(stderr, "After create (dp): %s\n", s);
	free(s);
	adt_free(dp);
	
	d.c = 'a';
	d.s = s_dup("[a] ready for duplication");
	if ((s = adt_tostring(&d)) == NULL)
		adt_err_sys(&d, "adt_creat error");
	fprintf(stderr, "About to dup (d): %s\n", s);
	free(s);
	if ((dp = adt_dup(&d)) == NULL)
		err_sys("adt_dup error");
	if ((s = adt_tostring(&d)) == NULL)
		adt_err_sys(&d, "adt_creat error");
	fprintf(stderr, "After dup (dp): %s\n", s);
	free(s);
	free(d.s);
	adt_free(dp);
	return 0;
}
