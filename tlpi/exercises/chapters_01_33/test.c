#include <sys/stat.h>
#include "tlpi_hdr.h"

int
main(void)
{
	struct stat *sbp;
	struct stat sb;

	setbuf(stdout, NULL);

	printf("%d %d\n", (int) sizeof(sbp), (int) sizeof(sb));
	exit(0);
	
}

	
