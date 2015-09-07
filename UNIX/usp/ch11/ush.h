#ifndef USH_H
#define USH_H

struct command {
	char **argv;
	char *infile;
	char *outfile;
};

/* parse.c */
struct command *cmd_init();
struct command *cmd_parse(const char *line);
void cmd_free(struct command *cmd);
void cmd_write(struct command *cmd);

/* exec.c */
int cmd_exec(struct command *cmd);


/* tst-parse.c */
int t_parse(const char *line);

#endif	/* USH_H */
