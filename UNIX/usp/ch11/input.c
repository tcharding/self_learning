#include "tch.h"
#include "ush.h"
#include <ctype.h>
#include <linux/limits.h>
#define BLANK " "
/*
 * USH - input.c
 *
 * Handle user input and parsing input into command structure. 
 * Also functions to manipulate command structure.
 */
static int argv_parse(struct command *cmd, const char *s);
static char *io_parse(struct command *cmd, const char *s);
static int pipeline_parse(struct command *cmd, const char *s);
static int process_redirect(struct command *cmd, const char *s);
static void writes_null(const char *s);
static void writev_null(char **v);
static struct command *cmd_init(void);

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
struct command *cmd_creat(const char *input)
{
	struct command *cmd;
	char *s;
	
	cmd = Malloc(sizeof(struct command));
	bzero(cmd, sizeof(struct command));

	if (strchr(input, '|') != '\0') {
		if (pipeline_parse(cmd, input) == -1) {
			cmd_free(cmd);
			DP("%s", "pipeline_parse error");
			return (struct command *)0;
		}
	} else {
				/* remove redirection options from input  */
		s = io_parse(cmd, input);

		if (argv_parse(cmd, s) == -1) {
			cmd_free(cmd);
			DP("%s", "argv_parse error");
			return (struct command *)0;
		}
	}
	free(s);
	return cmd;
}


/* cmd_free: free memory allocated with cmd_creat */
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
	if (cmd != NULL) {
		msg("struct command:\n");
		msg("\targv: ");
		writev_null(cmd->argv);
		msg("\n");
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

/* argv_parse: build NULL terminated argument vector from s */
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
/* io_parse: store copy of infile and outfile from s into cmd
   return newly allocated string with redirection removed */
static char *io_parse(struct command *cmd, const char *s)
{
	FILE *stream;
	size_t size;
	char *buf;

	if (s == NULL) {
		errno = EINVAL;
		return NULL;
	}
	buf = NULL, size = 0;
	if ((stream = open_memstream(&buf, &size)) == NULL)
		return NULL;
	while (*s != '\0') {
		if (*s == '<' || *s == '>') {
			s += process_redirect(cmd, s); /* moves s over redirect */
		} else {
			(void)fputc(*s, stream);
			++s;
		}
	}
	(void)fclose(stream);
	return buf;
}
/* process_redirect: add copy of file name to cmd
   return length of redirect i.e '< in' returns: 4 */ 
static int process_redirect(struct command *cmd, const char *s)
{
	FILE *stream;
	size_t size;
	char *buf;
	int len;
	char d;
	
	if (s == NULL) {
		errno = EINVAL;
		return NULL;
	}
	buf = NULL, size = 0;
	if ((stream = open_memstream(&buf, &size)) == NULL)
		return NULL;
	len = 0;
	d = *s;
	++s, ++len;
	while(isspace(*s))
		++s, ++len;
	while((!isspace(*s)) && *s != '\0') {
		fputc(*s, stream);
		++s, ++len;
	}
	(void)fclose(stream);
	switch(d) {
	case '<':
		if (cmd->infile != NULL) {
			err_msg("ush does not support multiple redirects"
			"within a pipeline, behaviour is undefined\n");
		}
		cmd->infile = buf;
		break;
	case '>':
		cmd->outfile = buf;
		break;
	default:
		DP("process_redirect: error, unknown char: %c", d);
		break;
	}
	return len;
}

/* pipeline_parse: parse pipeline command string s into cmd */
static int pipeline_parse(struct command *cmd, const char *s)
{
	if (s == NULL || cmd == (struct command *)0) {
		errno = EINVAL;
		return -1;
	}
	return makeargv(s, "|", &cmd->pipev);
}
/* writes_null: write possible null string to stderr */
static void writes_null(const char *s)
{
	if (s == NULL)
		fprintf(stderr, "<NULL>");
	else
		fprintf(stderr, "%s", s);
}
/* writev_null: write possible null vector to stderr */
static void writev_null(char **v)
{
	msg("[");
	if (v == NULL) {
		msg("<NULL>");	
	} else {
		v = v;
		while (*v != NULL) {
			msg("[%s]", *v);
			if (*(v+1) != NULL)
				msg(" ");
			v++;
		}
	}
	msg("]");	
}
static struct command *cmd_init(void)
{
	struct command *cmd;
	
	cmd = Malloc(sizeof(struct command));
	bzero(cmd, sizeof(struct command));
	return cmd;
}
       
/* -------- Unit tests ------------ */
static void t_argv_parse(void);
static void t_io_parse(void);
static void t_pipeline_parse(void);

/* unit test runner */
int input_unit_tests(void)
{
	t_argv_parse();
	t_io_parse();
	t_pipeline_parse();
	return 0;
}

static void t_argv_parse(void)
{
	struct command *cmd;
	char **dp;
	char *data[] = {
		"command",
		"command arg1 arg2",
		NULL
	};
	fprintf(stderr, "\n--- t_argv_parse ---\n");	

	for (dp = data; *dp != NULL; dp++) {
		fprintf(stderr, "line: |%s|\n", *dp);
		if ((cmd = cmd_init()) == (struct command *)0)
			err_quit("cmd_init");
		if (argv_parse(cmd, *dp) == -1)
			err_sys("argv_parse error");
		fprintf(stderr, "argv: ");
		writev_null(cmd->argv);
		fprintf(stderr, "\n");
		cmd_free(cmd);
	}
}

static void t_io_parse(void)
{
	struct command *cmd;
	char **dp, *s;
	char *data[] = {
		"<infile command arg1 arg2",
		"< infile command arg1 arg2",
		">outfile command arg1 arg2",
		"> outfile command arg1 arg2",
		">outfile <infile command arg1 arg2",
		"<infile >outfile command arg1 arg2",
		"> outfile < infile command arg1 arg2",
		"< infile > outfile command arg1 arg2",
		"command arg1 arg2 <infile >outfile",
		"command arg1 arg2 >outfile <infile",
		"command arg1 <infile arg2",
		"command <infile arg1 > outfile arg2",
		NULL
	};
	fprintf(stderr, "\n--- t_io_parse ---\n");	

	for (dp = data; *dp != NULL; dp++) {
		fprintf(stderr, "pre: |%s|\n", *dp);
		if ((cmd = cmd_init()) == (struct command *)0)
			err_quit("cmd_init");
		if ((s = io_parse(cmd, *dp)) == NULL)
			err_sys("io_parse error");
		fprintf(stderr, "post: %s\n", s);
		fprintf(stderr, "infile: %s\n", cmd->infile);
		fprintf(stderr, "outfile: %s\n", cmd->outfile);
		fprintf(stderr, "\n");
		free(s);
		cmd_free(cmd);
	}
	
}

static void t_pipeline_parse(void)
{
	struct command *cmd;
	char **dp;
	char *data[] = {
		"cmd | cmd1",
		"<in cmd | cmd1 arg >out",
		"<in cmd | cmd2 |cmd1 arg >out",
		NULL
	};
	fprintf(stderr, "\n--- t_pipelie_parse ---\n");	

	for (dp = data; *dp != NULL; dp++) {
		fprintf(stderr, "line: |%s|\n", *dp);
		if ((cmd = cmd_init()) == (struct command *)0)
			err_quit("cmd_init");
		if (pipeline_parse(cmd, *dp) == -1)
			err_sys("pipeline_parse error");
		fprintf(stderr, "pipev: ");
		writev_null(cmd->pipev);
		fprintf(stderr, "\n");
		cmd_free(cmd);
	}	
}
