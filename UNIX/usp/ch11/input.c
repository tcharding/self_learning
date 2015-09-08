#include "tch.h"
#include "ush.h"
#include <linux/limits.h>
#define BLANK " "

static int argv_parse(struct command *cmd, const char *s);
static void writes_null(const char *s);

/* getinput: read input from stdin, returned memory allocated with malloc */
char *getinput(void)
{
	char buf[MAX_CANON];
	char *s;
	
	/* for now just one line */
	(void)fgets(buf, MAX_CANON, stdin);
	if (*(buf + strlen(buf)-1) == '\n')
		*(buf + strlen(buf)-1) = '\0';
	s = s_dup(buf);
	return s;
}

/* parse: pares input line into cmd
    free with free_cmd() */
struct command *cmd_creat(const char *line)
{
	struct command *cmd;

	cmd = Malloc(sizeof(struct command));
	bzero(cmd, sizeof(struct command));

	if (argv_parse(cmd, line) == -1) {
		cmd_free(cmd);
		DP("%s", "cmd_free error");
		return (struct command *)0;
	}
	
	return cmd;
}


/* cmd_free: free memory allocated with cmd_init */
void cmd_free(struct command *cmd)
{
	if (cmd != NULL) {
		if (cmd->argv != NULL) {
			if (*cmd->argv != NULL)
				free(*cmd->argv); /* dependant on argv_parse  */
			free(cmd->argv);
		}
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
	char **v;
	
	if (cmd != NULL) {
		msg("struct command:\n");
		msg("\targv: [");
		if (cmd->argv == NULL) {
			msg("<NULL>");	
		} else {
			v = cmd->argv;
			while (*v != NULL) {
				msg("[%s]", *v);
				if (*(v+1) != NULL)
					msg(" ");
				v++;
			}
		}
		msg("]\n");
		msg("\tinfile: ");
		writes_null(cmd->infile);
		msg("\n");
		msg("\toutfile: ");
		writes_null(cmd->outfile);
		msg("\n");
	} else {
		msg("cmd_write error: argument is null\n");		
	}
}

/* argv_parse: build NULL terminate argument vector from s */
static int argv_parse(struct command *cmd, const char *s)
{
	int error, i, ntokens;
	const char *snew;
	char *t;
	char *delim = " ";
	char **v;

	if (s == NULL) {
		errno = EINVAL;
		return -1;
	}
	snew = s + strspn(s, delim); /* snew is real start of string */
	if ((t = malloc(strlen(snew) + 1)) == NULL)
		return -1;
	strcpy(t, snew);
	ntokens = 0;
	if (strtok(t, delim) != NULL) /* count the number of tokens */
		for (ntokens = 1; strtok(NULL, delim) != NULL; ntokens++)
			;
				/* create argument array for ptrs to the tokens */
	if ((v = calloc((size_t)(ntokens + 1), sizeof(char *))) == NULL) {
		error = errno;
		free(t);
		errno = error;
		return -1;
	}
				/* insert ptrs to tokens into the argument array */
	if (ntokens == 0)
		free(t);
	else {
		strcpy(t, snew);
		*v = strtok(t, delim);
		for (i = 1; i < ntokens; i++)
			*(v + i) = strtok(NULL, delim);
	}
	*(v + ntokens) = NULL; /* add final NULL pointer */
	cmd->argv = v;
	return ntokens;
}

/* writes_null: write possible null string to stderr */
static void writes_null(const char *s)
{
	if (s == NULL)
		fprintf(stderr, "<NULL>");
	else
		fprintf(stderr, "%s", s);
}
