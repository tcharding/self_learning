#include "tch.h"
#include "virtualtimers.h"

/* run tests */
int main(void)
{
	int nfail;

	nfail = 0;
	/* unit tests */
	nfail = virtt_unit_tests();
	if (nfail != 0)
		fprintf(stderr, "Tests failed: %d ( vt_unit_tests )", nfail);
	/* integration tests */
	system("./tst-virtualtimers");
	
	return 0;
}
