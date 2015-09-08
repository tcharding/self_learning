#include "tch.h"
#include "ush.h"
#include <fcntl.h>
#define FFLAG (O_WRONLY | O_CREAT | O_TRUNC)
#define FMODE (S_IRUSR | S_IWUSR)
/*
 * USH - exec.c
 *
 * Handle execution of command structure. Includes redirection and pipelines.
 */
static void exec_simple(struct command *cmd);
static void exec_redirect(struct command *cmd, int in, int out);
static void exec_pipeline(struct command *cmd);

/* execute: entry point for command execution functions */
void execute(struct command *cmd)
{
	if (cmd == (struct command *)0)
		return;
	else if (cmd->pipev != NULL)
		exec_pipeline(cmd);
	else if (cmd->infile != NULL || cmd->outfile != NULL)
		exec_redirect(cmd, 1, 1);
	else
		exec_simple(cmd);
}
/* exec_simple: execute simple command (cmd->argv only) */
static void exec_simple(struct command *cmd)
{
				/* execute the command */
	(void)execvp(cmd->argv[0], cmd->argv);
	exit(EXIT_FAILURE);	/* shouldn't get here */
	
}

/* exec_redirect: redirect input/output and execute command */
static void exec_redirect(struct command *cmd, int in, int out)
{
	int infd, outfd;
	int err;
				/* redirect input */
	if (in && cmd->infile != NULL) {
		if ((infd = open(cmd->infile, O_RDONLY)) == -1) {
			DP("open error for file: %s", cmd->infile);
			return;
		}
		if (dup2(infd, STDIN_FILENO) == -1) {
			err = errno, close(infd), errno = err;
			DP("%s", "dup2 error");
			return;
		}
		(void)close(infd);
	}
				/* redirect output */
	if (out && cmd->outfile != NULL) {
		if ((outfd = open(cmd->outfile, FFLAG, FMODE)) == -1) {
			DP("open error for file: %s", cmd->outfile);
			return;
		}
		if (dup2(outfd, STDOUT_FILENO) == -1) {
			err = errno, close(outfd), errno = err;
			DP("%s", "dup2 error");
			return;
		}
		(void)close(outfd);
	}
				/* execute the command */
	(void)execvp(cmd->argv[0], cmd->argv);
	exit(EXIT_FAILURE);	/* shouldn't get here */
}
/* exec_pipeline: re-parse cmd->pipev and exec each command */
static void exec_pipeline(struct command *cmd)
{
	int pid;
	int count;
	int i;
	int fd[2];
	char **v;
	struct command *pipecmd;

	if (cmd == (struct command *)0 || cmd->pipev == NULL) {
		errno = EINVAL;
		return;
	}
			/* count commands in pipeline */
	count = 0;
	for (v = cmd->pipev; *v != NULL; ++v)
		++count;
	for (i = 0; i < count-1; i++) { /* handle all but last one */
		if (pipe(fd) == -1) {
			DP("%s", "pipe error");
			return;
		}
		else if ((pid = fork()) == -1) {
			DP("%s", "fork error");
			return;
		} else if (pid > 0) { /* parent */
			if (dup2(fd[1], STDOUT_FILENO) == -1) {
				DP("%s", "dup2 error");
				return;
			}
			if (close(fd[0]) || close(fd[1])) {
				DP("%s", "close error");
				return;
			}
			pipecmd = cmd_creat(cmd->pipev[i]);
			exec_redirect(pipecmd, i==0, 0); /* does not return */
		} else {	/* child */
			if (dup2(fd[0], STDIN_FILENO) == -1) {
				DP("%s", "dup2 error");
				return;
			}
			if (close(fd[0]) || close(fd[1])) {
				DP("%s", "close error");
				return;
			}
		}
	}
				/* handle last one */
	pipecmd = cmd_creat(cmd->pipev[count-1]);
	exec_redirect(pipecmd, 0, 1); /* does not return */
}
