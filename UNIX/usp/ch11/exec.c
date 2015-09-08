#include "tch.h"
#include "ush.h"
#include <fcntl.h>
#define FFLAG (O_WRONLY | O_CREAT | O_TRUNC)
#define FMODE (S_IRUSR | S_IWUSR)

/* cmd_exec: execute command */
int cmd_exec(struct command *cmd)
{
	int infd, outfd;
	int err;
				/* redirect input */
	if (cmd->infile != NULL) {
		if ((infd = open(cmd->infile, O_RDONLY)) == -1) {
			DP("open error for file: %s", cmd->infile);
			return -1;
		}
		if (dup2(infd, STDIN_FILENO) == -1) {
			err = errno, close(infd), errno = err;
			DP("%s", "dup2 error");
			return -1;
		}
		(void)close(infd);
	}
				/* redirect output */
	if (cmd->outfile != NULL) {
		if ((outfd = open(cmd->outfile, FFLAG, FMODE)) == -1) {
			DP("open error for file: %s", cmd->outfile);
			return -1;
		}
		if (dup2(outfd, STDOUT_FILENO) == -1) {
			err = errno, close(outfd), errno = err;
			DP("%s", "dup2 error");
			return -1;
		}
		(void)close(outfd);
	}
				/* execute the command */
	(void)execvp(cmd->argv[0], cmd->argv);
	exit(EXIT_FAILURE);	/* shouldn't get here */
	return 0;
}
