#ifndef USH_H
#define USH_H

#define DEBUG 1
#define DP(fmt, ...) if (DEBUG) fprintf(stderr, fmt, __VA_ARGS__);

struct command {
	char **argv;		/* for simple command */
	char *infile;		/* redirect stdin */
	char *outfile;		/* redirect stdout */
	char **pipev;		/* pipeline command */
	int background;
};

/* input.c */
char *getinput(void);
struct command *cmd_creat(const char *input);
void cmd_free(struct command *cmd);
void cmd_write(struct command *cmd);
int input_unit_tests(void);

/* exec.c */
void execute(struct command *cmd);

/* tst-parse.c */
int t_parse(const char *line);

#endif	/* USH_H */
