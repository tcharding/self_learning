#include "tch.h"
#include "ush.h"
#define BLANK " "

static void writev(char **v);

/* parse: pares input line into cmd
    free with free_cmd() */
struct command *cmd_parse(const char *line)
{
	struct command *cmd;
	char *dup;
	int ntokens;
	char *v;
	

	ntokens = count_tokens(line);
	
	dup = s_dup(line);	/* allocates new memory */


	cmd = cmd_init();
	
	return cmd;
}

/* cmd_init: allocate and zero memory */
struct command *cmd_init()
{
	struct command *cmd;

	cmd = Malloc(sizeof(struct command));
	bzero(cmd, sizeof(struct command));
	return cmd;
}

/* cmd_free: free memory allocated with cmd_init */
void cmd_free(struct command *cmd)
{
	if (cmd != NULL) {
		if (cmd->argv != NULL)
			free(cmd->argv);
		if (cmd->infile != NULL)
			free(cmd->infile);
		if (cmd->outfile != NULL)
			free(cmd->outfile);
		free(cmd);
	}
}
/* cmd_write: pretty print cmd to sdterr */
void cmd_write(struct command *cmd)
{
	if (cmd != NULL) {
		msg("<struct command>\n");
		if (cmd->argv != NULL) {
			msg("\targv: ");
			writev(cmd->argv);
			msg("\n");
		}
		if (cmd->infile != NULL)
			msg("\tinfile: %s\n", cmd->infile);
		if (cmd->outfile != NULL)
			msg("\toutfile: %s\n", cmd->outfile);
	} else
		msg("cmd_write error: argument is null\n");
}

/* writev: write NULL terminated vector of strings to stderr */
static void writev(char **v)
{
	msg("[");	
	if (v == NULL) {
		msg("<NULL>");	
	} else {
		for ( ; *v != NULL; v++) {
			msg("[%s] ", *v);
		}
	}
	msg("]");
}
