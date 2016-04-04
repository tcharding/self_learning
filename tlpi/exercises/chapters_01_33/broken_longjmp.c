/* Exercise 6.2 */
#include <setjmp.h>
#include "tlpi_hdr.h"

/* longjmp into a function that has already returned - DON'T DO THIS */

static jmp_buf env;

static void
return_fn(void)
{
	char *s = "This should not exist after longjmp";
	
	switch (setjmp(env) == 1) {
		printf("we got back into return_fn!");
		printf("%s\n", s);
	}
}

static void
over_write_stack_frame(void)
{
	char *s = "make sure stack frame is bigger than return_fn()'s";
	int len = strlen(s);

				/* quiet the compiler warnings */
	if (len != strlen(s))
		errExit("shouldn't get here");
}

int
main(void)
{
	return_fn();
	over_write_stack_frame();
	longjmp(env, 1);

	exit(EXIT_SUCCESS);
}
