#include "tch.h"
#include "vector.h"
#include "data.h"
#include "string.h"
#include <ctype.h>

static int parse_config(const char *file, vec_t *v);

/* create ring of n processes */
int main(int argc, char *argv[])
{
	int fd[2];
	pid_t pid;
	int i, status, nproc;
	int n;			/* number of stages, also lines in config file */
	char *infile, *outfile, *config;
	int nid, res;
	vec_t v;		/* transformations from config file */

	v.slots = 0, v.cnt = 0, v.data = NULL; /* initialise v */
	n = nproc = pid = status = fd[0] = fd[1] = 0;
	if (argc != 5) 
		err_quit("Uasge: %s stages config.in file.in file.out", argv[0]);
	if ((n = atoi(argv[1])) < 0)
		err_sys("atoi failed with %s", argv[1]);
	config = argv[2], infile = argv[3], outfile = argv[4];
	nproc = n + 2;
	nid = 0;		/* node ID of initial process is 0 */

	if ((res = parse_config(config, &v)) != n)
		err_sys("parse_config error: res: %d n: %d", res, n);

	if (v_foreach(&v, adt_pprint) == -1)
		err_sys("print vector error");
	
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
	
	fprintf(stderr, "nid: %d, pid: %ld ppid: %ld, c: %c s: %s\n",
		nid , (long)getpid(), (long)getppid(), v.data[n]->c, v.data[n]->s);

	return 0;
}

/* parse_config: parse config file putting transformation's into v */
static int parse_config(const char *file, vec_t *v)
{
	data_t *d;
	char *rdline, *ptr;
	int lineno;
	FILE *stream;
	size_t len;
	ssize_t read;
	int cnt;

	cnt = 0;
	lineno = 0;
	rdline = NULL, len = 0;

	if ((stream = fopen(file, "r")) == NULL)
		err_sys("fopen failed with %s", file);
	
	while ((read = getline(&rdline, &len, stream)) != -1) {
		lineno++;
		if (*rdline == '#') {
			/* free(rdline); */
			continue; /* skip comment lines */
		}
		if ((d = adt_alloc()) == NULL) 
			err_quit("adt_alloc error");

		ptr = rdline;	
		if (*(ptr + strlen(ptr) - 1) == '\n') /* remove line feed */
			*(ptr + strlen(ptr) - 1) = '\0';

		while (isspace(*ptr++))	/* skip whitespace */
			;	
		if (*ptr == '\0') { /* skip blank lines */
			/* free(rdline); */
			adt_free(d);
			continue; 
		}
		cnt++;		/* testing */
		d->c = *ptr;
		while (isspace(*++ptr))	/* skip whitespace */
			;	
		if (*ptr == '\0') {
			err_msg("ringpp: config read error an line: %d\n", lineno);
			adt_free(d);
			/* free(rdline); */
			return -1;
		}
		d->s = s_dup(ptr); /* allocates new memory */
		if (v_add(v, d) != cnt)
			err_sys("v_add error");
	}
	
	if (ferror(stream)) 
		err_msg("get_trans: stream error");
	if (fclose(stream) != 0)
		err_msg("fclose error");
	fprintf(stderr, "END cnt: %d v->cnt: %d\n", cnt, v->cnt);
	return v->cnt;
}
