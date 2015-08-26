#include "tch.h"

/* processchar: read fdin, write fdout, replace all occurrences of c with s */
int processchar(int fdin, int fdout, char c, const char *s)
{
	char readc;
	int retval;
	
	while ((retval = read(fdin, &readc, 1)) == 1) {
		if (((readc == c) ?
		     write(fdout, s, strlen(s)) :
		     write(fdout, &readc, 1)) == -1)
			return -1;
	}
	return retval;
}
