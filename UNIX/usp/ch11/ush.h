#ifndef USH_H
#define USH_H

struct command {
	char **argv;
	char *infile;
	char *outfile;
};

#define DEBUG 1
#define DP(fmt, ...) if (DEBUG) fprintf(stderr, fmt, __VA_ARGS__);

/* input.c */
char *getinput(void);
struct command *cmd_creat(const char *line);
void cmd_free(struct command *cmd);
void cmd_write(struct command *cmd);
int input_unit_tests(void);

/* exec.c */
int cmd_exec(struct command *cmd);

/* tst-parse.c */
int t_parse(const char *line);

#endif	/* USH_H */
