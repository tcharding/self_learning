#include "tch.h"
#include "ush.h"
#include <linux/limits.h>
#include <setjmp.h>
#define PROMPT "ush> "
/*
 * USH - Ultra-simple SHell
 * 
 * Based on chapter 11 of UNIX Systems Programming - Robbins and Robbins
 *
 * Tobin Harding 2015
 */
static void prompt(void);
void sig_int(int);
static sigjmp_buf jmpbuf;

int main(void)
{
	char *input;
	struct command *cmd;
	pid_t pid;

	(void)Signal(SIGINT, sig_int);
	
	for ( ; ; ) {
		if (sigsetjmp(jmpbuf, 1)) {
			fputs("\n", stdout);
			continue;
		}
		prompt();
		if ((input = getinput()) == NULL) {
			DP("%s", "getinput error");
			continue; /* read error */
		}

		if ((cmd = cmd_creat(input)) == NULL) {
			DP("%s", "cmd_creat error");
			continue;
		}
		if ((pid = fork()) < 0) {
			DP("%s", "fork error");
			continue;
		} else if (pid == 0) { /* child */
			/* cmd_write(cmd); */
			execute(cmd);			
		} else {
			if (cmd->background == 0)
				(void)wait(NULL);
		}
		
		cmd_free(cmd);
	}
	return 0;
}

/* prompt: print prompt to stdout */
static void prompt(void)
{
	fprintf(stdout, "%s", PROMPT);
}
/* sig_int: catch SIGINT */
void sig_int(int signo)
{
	siglongjmp(jmpbuf, 1);
}
