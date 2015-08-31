#include "apue.h"
#include <stdarg.h>

char *sig2string(int signo, char *str);
void msg(const char *fmt, ...);

/* test sig2string */
int main(int argc, char *argv[])
{
	int n;
	char buf[BUFSIZ];
	
	if (argc != 2)
		err_quit("Usage: %s signo", argv[0]);
	if ((n = atoi(argv[1])) < 0)
		err_sys("atoi error");
	if (sig2string(n, buf) == NULL)
		err_sys("sig2string error");
	
	msg("signo: %d s: %s\n", n, buf);
	return 0;
}

/* sig2string: convert signal number to string */
char *sig2string(int signo, char *str)
{
	extern char *sys_siglist[];
	
	if (sprintf(str, "%s", sys_siglist[signo]) < 0)
		return NULL;
	return str;
}

void msg(const char *fmt, ...)
{
	va_list		ap;

	va_start(ap, fmt);
	vfprintf(stderr, fmt, ap);
	va_end(ap);
}

