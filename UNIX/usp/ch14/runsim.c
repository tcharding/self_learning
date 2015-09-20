#include "tch.h"
#include "license.h"
#include <sys/types.h>
#include <sys/wait.h>
#include <limits.h>

static int docommand(const char *cmd);
static void local_writev(char **v);

int main(int argc, char *argv[])
{
	int status, retval, n;
	char rdline[MAX_CANON];
	pid_t pid;

	if (argc != 2)
		err_quit("Usage: %s n\n", argv[0]);
	n = atoi(argv[1]);
	status = 0;
	if (initlicense() == -1)
		err_quit("initlicense error");
	if (addtolicense(n) == -1)
		err_quit("addtolicense error");
	
	while (fgets(rdline, MAX_CANON, stdin) != NULL) {
		if (getlicense() == -1)
			err_sys("getlicense error");
	
		if (*(rdline + strlen(rdline) - 1) == '\n')
			*(rdline + strlen(rdline) - 1) = '\0';

		if ((pid = Fork()) == 0) { /* child */
			retval = docommand(rdline);
			exit(retval);
		} else {	/* parent */
			pid = waitpid(-1, &status, WNOHANG);
			exit(status);
		}
	}
	if (ferror(stdin) != 0)
		err_sys("read error");
	return 0;
}

static int docommand(const char *cline)
{
	char **argv;
	pid_t pid;

	if (cline == NULL) {
		errno = EINVAL;
		return -1;
	}
	
	if ((pid = Fork()) == 0) { /* child */
		if (makeargv(cline, " ", &argv) == -1)
			err_sys("makeargv error");
		local_writev(argv);
		if (execvp(argv[0], argv) == -1)
			err_sys("execvp error");
	} else {		/* parent */
		if (wait(NULL) == -1)
			err_sys("wait error");
		if (returnlicense() == -1)
			err_sys("returnlicense error");
		exit(0);
	}
	return -1;		/* shouldn't get here */
}
/* writev: write v to stdout */
static void local_writev(char **v)
{
	if (v) {
		printf("start:|");
		while(*v)
			printf(" %s |", *v++);
		printf("end\n");
	}
}

