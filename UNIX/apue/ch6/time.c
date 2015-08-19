#include "apue.h"
#include <time.h>
#define BUFSIZE 64

int main(void)
{
	time_t t;
	struct tm *tmp;
	char buf[BUFSIZE];
	char *time_zone;

	time_zone = getenv("TZ");
	(void)time(&t);
	tmp = localtime(&t);
	if (strftime(buf, BUFSIZE, "%c", tmp) == 0)
		err_msg("Buffer leng too small: %d", BUFSIZE);
	fprintf(stdout, "%s\n", buf);
	fprintf(stdout, "%s\n", time_zone);
	
	return 0;
}
