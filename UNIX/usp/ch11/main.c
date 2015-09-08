#include "tch.h"
#include "ush.h"
#include <linux/limits.h>
#define PROMPT "ush> "

static void prompt(void);

int main(void)
{
	char *input;
	struct command *cmd;
	
	for ( ; ; ) {
		prompt();
		if ((input = getinput()) == NULL)
			continue; /* read error */
		if ((cmd = cmd_creat(input)) == NULL)
			continue;
		cmd_write(cmd);
		cmd_free(cmd);
	}
	return 0;
}

/* prompt: print prompt to stdout */
static void prompt(void)
{
	fprintf(stdout, "%s", PROMPT);
}
