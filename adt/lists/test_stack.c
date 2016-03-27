#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>

#include "stack.h"

#ifndef BUF_SIZE
#define BUF_SIZE 1064
#endif

static void fail(const char *msg, const char *exp, const char *got);
static void simple(void);
static void multiple(void);

struct stack *sp;

int
main(void)
{
	simple();

	exit(EXIT_SUCCESS);
}

/* simple: initial test */
static void simple(void)
{
	char *tc;
	char buf[BUF_SIZE];

	bzero(buf, sizeof(buf));
	
	tc = "push this on";
	if (push(sp, tc)) {
		fprintf(stderr, "push");
		exit(EXIT_FAILURE);
	}

	if (show(sp, buf, BUF_SIZE) == NULL) {
		fprintf(stderr, "show");
		exit(EXIT_FAILURE);
	}
	if (strcmp(buf, tc) != 0)
		fail("show", tc, buf);

	bzero(buf, sizeof(buf));

	if (pop(sp, buf, BUF_SIZE) == NULL) {
		fprintf(stderr, "pop");
		exit(EXIT_FAILURE);
	}
	if (strcmp(buf, tc) != 0)
		fail("pop", tc, buf);

	if (pop(sp, buf, BUF_SIZE) != NULL)
		fprintf(stderr, "pop empty stack");

}

static void
multiple(void)
{
	
}

/* fail: test fail message */
static void
fail(const char *msg, const char *exp, const char *got)
{
	fprintf(stderr, "Fail: %s exp: %s got: %s\n", msg, exp, got);
}
