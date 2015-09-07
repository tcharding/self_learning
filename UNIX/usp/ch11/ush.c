#include "tch.h"
#include "ush.h"
#include <linux/limits.h>
#include <readline/readline.h>
#include <readline/history.h>
#define MAX_BUFFER 256
#define PROMPT "ush> "

int exec_cmd(char *incmd);	/* defined in exec-cmd.c */

	
int main(void)
{
	struct command *cmd;
	char *rdline;
	pid_t pid;

	for ( ; ; ) {
		if ((rdline = readline(PROMPT)) == NULL)
			continue;
		if ((cmd = cmd_parse(rdline)) == NULL)
			continue;
		if ((pid = fork()) < 0)
			err_sys("fork errror");
		else if (pid == 0) {
			(void)cmd_exec(cmd);
			return 1;
		} else
			(void)wait(NULL);
		/* free(cmd->argv); */
		/* free(rdline); */
	}
	return 0;
}
