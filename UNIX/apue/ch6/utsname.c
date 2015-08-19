#include "apue.h"
#include <sys/utsname.h>


/* print utsname info; mimits `uname -a` in the shell */
int main(void)
{
	struct utsname u;

	bzero(&u, sizeof(struct utsname));
	if (uname(&u) == -1)
		err_sys("utsname failed");

	fprintf(stdout, "%s %s %s %s %s\n",
		u.sysname, u.nodename, u.release, u.version, u.machine);
	return 0;
}
