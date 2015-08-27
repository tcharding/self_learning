#include "tch.h"
#include "vector.h"
#include "data.h"
#include "string.h"
#include <ctype.h>


static data_t **parse_config(const char *file);
static void write_tp(data_t **tp);

/* create ring of n processes */
int main(int argc, char *argv[])
{
	int fd[2];
	pid_t pid;
	int i, status, nproc;
	int n;			/* number of stages, also lines in config file */
	char *infile, *outfile, *config;
	int nid;
	data_t **tp;		/* array of transformations */
	
	n = nproc = pid = status = fd[0] = fd[1] = 0;
	if (argc != 5) 
		err_quit("Uasge: %s stages config.in file.in file.out", argv[0]);
	if ((n = atoi(argv[1])) < 0)
		err_sys("atoi failed with %s", argv[1]);
	config = argv[2], infile = argv[3], outfile = argv[4];
	nproc = n + 2;
	nid = 0;		/* node ID of initial process is 0 */

	if ((tp = parse_config(config)) == NULL)
		err_sys("parse_config error");

	write_tp(tp);
				/* create initial ring */
	Pipe(fd);
	Dup2(fd[0], STDIN_FILENO);
	Dup2(fd[1], STDOUT_FILENO);
	Close(fd[0]);
	Close(fd[1]);
				/* add process to ring */
	for (i = 1; i < nproc; i++) {
		Pipe(fd);
		if ((pid = Fork()) > 0)
			Dup2(fd[1], STDOUT_FILENO);
		else {
			Dup2(fd[0], STDIN_FILENO);
			nid = i;
		}
		Close(fd[0]);
		Close(fd[1]);
		if (pid > 0)
			break;
	}

	if (pid > 0)
		if (wait(&status) == -1)
			err_sys("wait error");
	
	fprintf(stderr, "This is process: %d, ID: %ld parent: %ld\n",
	       nid , (long)getpid(), (long)getppid());

	return 0;
}

/* parse_config: parse config file returning array of transformations */
static data_t **parse_config(const char *file)
{
	VECTOR *v;
	data_t *d, **tp;
	char *rdline, *ptr;
	int lineno;
	size_t n;
	FILE *fp;
	
	lineno = 0;
	rdline = NULL;
	if ((v = v_creat()) == NULL)
		err_quit("v_creat error");

	if ((fp = fopen(file, O_RDONLY)) == NULL)
			return NULL;

	while (getline(&rdline, &n, fp) != -1) {
		lineno++;
		if ((d = adt_alloc()) == NULL)
			err_quit("adt_creat error");
		ptr = rdline;	
		if (*ptr == '#') {
			free(rdline);
			adt_free(d);
			continue; /* skip comment lines */
		}
		if (*(rdline + strlen(rdline) - 1) == '\n') /* remove line feed */
			*(rdline + strlen(rdline) - 1) = '\0';
		while (isspace(*ptr++))	/* skip whitespace */
			;	
		if (*ptr == '\0') { /* skip blank lines */
			free(rdline);
			adt_free(d);
			continue; 
		}
		d->c = *ptr;
		while (isspace(*++ptr))	/* skip whitespace */
			;	
		if (*ptr == '\0') {
			err_msg("ringpp: config read error an line: %d\n", lineno);
			adt_free(d);
			free(rdline);
			return NULL;
		}
		d->s = s_dup(ptr); /* allocates new memory */
		if (v_add(v, d) < 0)
			err_sys("v_add error");
		free(rdline);
	}
	if (ferror(fp)) 
		err_msg("get_trans: stream error");
	tp = v->data;		/* sneaky! we are returning part of v */
	free(v);		/* so don't use v_free */
	return tp;

	
}
/* write_tp: write contents of tp to stderr */
static void write_tp(data_t **tp)
{
	char *s;
	data_t *dp;

	fprintf(stderr, "writing tp\n");
	for (dp = *tp; dp != NULL; dp = ++*tp) {
		s = adt_tostring(dp);
		fprintf(stderr, "%s", s);
		free(s);
	}
	fprintf(stderr, "\n");
}
