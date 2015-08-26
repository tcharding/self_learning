#include "tch.h"
#include "string.h"
#include "vector.h"
#include <ctype.h>

typedef struct transformation {
	char c;
	char *s;
} trans_t;

static int get_trans(trans_t **t, const char *file);

/* create ring of n processes */
int main(int argc, char *argv[])
{
	int fd[2];
	pid_t pid;
	int i, status, nproc;
	int n;			/* number of stages, also lines in config file */
	char *infile, *outfile, *config;
	int nid;
	trans_t *t;		/* array of transformations */
	
	n = nproc = pid = status = fd[0] = fd[1] = 0;
	if (argc != 5) 
		err_quit("Uasge: %s stages config infile outfile", argv[0]);
	if ((n = atoi(argv[1])) < 0)
		err_sys("atoi failed with %s", argv[1]);
	config = argv[2], infile = argv[3], outfile = argv[4];
	nproc = n + 2;
	nid = 0;		/* node ID of initial process is 0 */

	if (get_trans(&t, config) == -1)
		err_sys("get_trans error");
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
/* get_trans: read file and populate t with transformations */
static int get_trans(trans_t **t, const char *file)
{
	FILE *fp;
	char *rdline;
	char *ptr;
	size_t n;
	trans_t *tbuf;
	int c, lineno;
	struct {
		int size;
		int count;
		trans_t *data;
	} tab;

	tab.size = 0;
	tab.count = 0;
	lineno = n = c = 0;
	bzero(&rdline, sizeof(char **)); /* quiet lint */
	if ((fp = fopen(file, O_RDONLY)) == NULL)
		return -1;

	while (getline(&rdline, &n, fp) != -1) {
		lineno++;
		ptr = rdline;	
		if (*ptr == '#') {
			free(rdline);
			continue; /* skip comment lines */
		}
		if (*(rdline + strlen(rdline) - 1) == '\n') /* remove line feed */
			*(rdline + strlen(rdline) - 1) = '\0';
		while (isspace(*ptr++))	/* skip whitespace */
			;	
		if (*ptr == '\0') { /* skip blank lines */
			free(rdline);
			continue; 
		}
		tbuf = Malloc(sizeof(trans_t));
		tbuf->c = *ptr;
		while (isspace(*++ptr))	/* skip whitespace */
			;	
		if (*ptr == '\0') {
			err_msg("ringpp: config read error an line: %d\n", lineno);
			free(rdline);
			return -1;
		}
		tbuf->s = dups(ptr); /* allocates new memory */
		v_add(tab, tbuf);
		free(rdline);
	}
	if (ferror(fp)) 
		err_msg("get_trans: stream error");
	*t = tab.data;
	return 0;
}
