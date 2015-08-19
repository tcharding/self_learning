#include <stdio.h>

/* setbuf: turn buffering on or off */
void setbuf(FILE *restrict fp, char *restrict buf)
{
	if (buf == NULL) {
		setvbuf(fp, buf, _IONBF, 0);
	} else {
		setvbuf(fp, buf, _IOFBF, BUFSIZ);
	}
}
