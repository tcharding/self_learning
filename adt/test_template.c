#include "tlpi_hdr.h"

static void fail(const char *msg, const char *exp, const char *got);
static void t_simple(void);

int
main(void)
{
	t_simple();

	exit(EXIT_SUCCESS);
}

/* t_simple: initial test */
static void t_simple(void)
{
	
}

/* fail: test fail message */
static void
fail(const char *msg, const char *exp, const char *got)
{
	fprintf(stderr, "Fail: %s exp: %s got: %s\n", msg, exp, got);
}
