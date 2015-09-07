#include "tch.h"
#include "ush.h"
#define BLANK " "


/* UNIX/lib/makeargv.c */
int makeargv(const char *s, const char *delim, char ***argvp); 

static void write_argv(char **argv);

/* cmd_init: initialise memory */
struct command *cmd_init()
{
	struct command *cmd;

	cmd = Malloc(sizeof(struct command));
	bzero(cmd, sizeof(struct command));
	/* cmd->argv = NULL; */
	/* cmd->infile = NULL; */
	/* cmd->outfile = NULL; */
	return cmd;
}
/* parse: pares input line into cmd
    free with free_cmd() */
struct command *cmd_parse(const char *line)
{
	struct command *cmd;
	char **chargv;

	chargv = NULL;
	cmd = cmd_init();
	if (makeargv(line, BLANK, &chargv) < 0) {
		cmd_free(cmd);
		err_msg("makeargv error");
		return NULL;
	}
	cmd->argv = chargv;
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
	}
}
/* cmd_write: pretty print cmd to sdterr */
void cmd_write(struct command *cmd)
{
	fprintf(stderr, "<struct command>: \n");
	fprintf(stderr, "\targv: ");
	write_argv(cmd->argv);
	fprintf(stderr, "\n");
	if (cmd->infile != NULL)
		fprintf(stderr, "\tinfile: %s\n", cmd->infile);
	if (cmd->outfile != NULL)
		fprintf(stderr, "\toutfile: %s\n", cmd->outfile);

}

static void write_argv(char **argv)
{
	if (argv == NULL) {
		fprintf(stderr, "[NULL]");
	} else {
		for ( ; *argv != NULL; argv++) {
			fprintf(stderr, "[%s] ", *argv);
		}
	}
}
