/* Exercise */
#include "tlpi_hdr.h"

int
main(int argc, char *argv) {


	if (argc < 2 || strcmp(argv[1], "--help") == 0)
		usageErr("%s ", argv[0]);
	
}
