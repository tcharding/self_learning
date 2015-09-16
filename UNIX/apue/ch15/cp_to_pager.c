#include "apue.h"
#include <sys/wait.h>

#define DEF_PAGER "/bin/more"	/* default pager program */
enum { IN, OUT};		/* pipe's */
	
int main(int argc, char *argv[])
{
	size_t len;
	ssize_t nwriten;
	int fd[2];
	pid_t pid;
	char *pager, *argv0;
	char line[MAXLINE];
	FILE *fp;

	bzero(&fd[IN], sizeof(int));
	bzero(&fd[OUT], sizeof(int));
	if (argc != 2)
		err_quit("Usage: %s <pathname>", argv[0]);

	if ((fp = fopen(argv[1], "r")) == NULL)
		err_sys("fopen error");
	Pipe(fd);
	if ((pid = Fork()) > 0)	{ /* parent */
		Close(fd[IN]);	  /* close read end */
		/* parent copies argv[1] to pipe */
		while (fgets(line, MAXLINE, fp) != NULL) {
			len = strlen(line);
			nwriten = write(fd[OUT], line, len);
			if ((nwriten < 0) || ((size_t)nwriten != len))
				err_sys("write to pipe error");
		}
		if (ferror(fp) != 0)
			err_sys("fgets error");
		Close(fd[OUT]);	/* close write end of pipe for reader */

		if (waitpid(pid, NULL, 0) < 0)
			err_sys("waitpid error");
		exit(0);
	} else {		/* child */
		Close(fd[OUT]);
		if (fd[IN] != STDIN_FILENO) { /* defensive programming */
			Dup2(fd[IN], STDIN_FILENO);
			Close(fd[IN]);
		}
		/* get arguments for execl */
		if ((pager = getenv("PAGER")) == NULL)
			pager = DEF_PAGER;
		if ((argv0 = strrchr(pager, '/')) != NULL)
			argv0++; /* step past rightmost slash */
		else
			argv0 = pager; /* no slash in pager */

		if (execl(pager, argv0, (char *)0) < 0)
			err_sys("execl error for %s", pager);
	}
	exit(0);
}
