#include "apue.h"

enum { IN, OUT };		/* pipe fd's */

static void	sig_pipe(int);		/* our signal handler */

int
main(void)
{
	int n, fd1[2], fd2[2];
	pid_t pid;
	char line[MAXLINE];
	FILE *infp, *outfp;

	if (signal(SIGPIPE, sig_pipe) == SIG_ERR)
		err_sys("signal error");

	if (pipe(fd1) < 0 || pipe(fd2) < 0)
		err_sys("pipe error");

	if ((pid = fork()) < 0) {
		err_sys("fork error");
	} else if (pid > 0) {	/* parent */
		close(fd1[IN]),	close(fd2[OUT]);

		if (((infp = fdopen(fd2[IN], "r")) == NULL) ||
		    ((outfp = fdopen(fd1[OUT], "w")) == NULL))  
			err_sys("fdopen error");
		
		if ((setvbuf(infp, NULL, _IOLBF, 0) != 0) ||
		    (setvbuf(outfp, NULL, _IOLBF, 0) != 0))
			err_sys("setvbuf error");
				
		while (fgets(line, MAXLINE, stdin) != NULL) {
			if (fputs(line, outfp) == EOF)
				err_sys("fputs error");
			if (fgets(line, MAXLINE, infp) == NULL) {
				if (ferror(infp) != 0)
					err_sys("fgets error");
				else
					err_msg("child closed pipe");
			}
			if (fputs(line, stdout) == EOF)
				err_sys("fputs error");
		}
		Fclose(infp);
		Fclose(outfp);
		if (ferror(stdin))
			err_sys("fgets error on stdin");
		exit(0);
	} else {		/* child */
		close(fd1[OUT]);
		close(fd2[IN]);
		if (fd1[IN] != STDIN_FILENO) {
			if (dup2(fd1[IN], STDIN_FILENO) != STDIN_FILENO)
				err_sys("dup2 error to stdin");
			close(fd1[IN]);
		}

		if (fd2[OUT] != STDOUT_FILENO) {
			if (dup2(fd2[OUT], STDOUT_FILENO) != STDOUT_FILENO)
				err_sys("dup2 error to stdout");
			close(fd2[OUT]);
		}
		if (execl("./add2", "add2", (char *)0))
			err_sys("execl error");
	}
	exit(0);
}

static void
sig_pipe(int signo)
{
	printf("SIGPIPE caught\n");
	exit(1);
}
