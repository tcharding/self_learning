#include "tlpi_hdr.h"

void
fn2(void)
{
	void fn1(void);		/* from one.c */
	
	printf("two: fn\n");
	fn1();
}
